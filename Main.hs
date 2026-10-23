{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (Handler (..), IOException, catches)
import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (intToDigit, isControl, ord)
import Data.List (intercalate)
import Numeric (showFFloat, showHex)
import GHC.IO.Encoding (setFileSystemEncoding)
import Relic.SGA
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO

------------------------------------------------------------------------
-- Command line

data Cmd
  = Info FilePath
  | Tree FilePath
  | Ls FilePath
  | Cat FilePath String
  | Unpack UnpackOpts

data UnpackMode = Auto | Merge | Isolate

data UnpackOpts = UnpackOpts
  { uoSrc     :: FilePath
  , uoOut     :: FilePath
  , uoMode    :: UnpackMode
  , uoForce   :: Bool
  , uoVerbose :: Bool
  }

usage :: String
usage =
  unlines
    [ "usage: sgaunpack -viewer COMMAND ..."
    , ""
    , "  info   FILE.sga                  archive metadata as JSON"
    , "  tree   FILE.sga                  print the directory tree"
    , "  ls     FILE.sga                  flat listing: size and path"
    , "  cat    FILE.sga PATH             write one file to stdout"
    , "  unpack [-m|-i] [-f] [-v] FILE.sga OUTDIR"
    , "                                   extract everything"
    , ""
    , "unpack options:"
    , "  -m, --merge    write all drives straight into OUTDIR"
    , "  -i, --isolate  write each drive to OUTDIR/<alias>/"
    , "                 (default: merge if there is one drive, else isolate)"
    , "  -f, --force    allow OUTDIR to already exist"
    , "  -v, --verbose  print every extracted file"
    , ""
    , "Paths in `ls` output (alias:dir/file) can be passed to `cat`."
    , "Only SGA version 10 is supported."
    ]

parseArgs :: [String] -> Either String Cmd
parseArgs ("info" : rest) = one Info rest
parseArgs ("tree" : rest) = one Tree rest
parseArgs ("ls" : rest) = one Ls rest
parseArgs ["cat", f, p] = Right (Cat f p)
parseArgs ("cat" : _) = Left "cat takes FILE.sga and PATH"
parseArgs ("unpack" : rest) = go (UnpackOpts "" "" Auto False False) [] rest
  where
    go o pos [] = case reverse pos of
      [s, d] -> Right (Unpack o {uoSrc = s, uoOut = d})
      _ -> Left "unpack takes FILE.sga and OUTDIR"
    go o pos (a : as)
      | a `elem` ["-m", "--merge"] = go o {uoMode = Merge} pos as
      | a `elem` ["-i", "--isolate"] = go o {uoMode = Isolate} pos as
      | a `elem` ["-f", "--force"] = go o {uoForce = True} pos as
      | a `elem` ["-v", "--verbose"] = go o {uoVerbose = True} pos as
      | take 1 a == "-" = Left ("unknown option " ++ a)
      | otherwise = go o (a : pos) as
parseArgs (c : _) = Left ("unknown command " ++ show c)
parseArgs [] = Left "missing command"

one :: (FilePath -> Cmd) -> [String] -> Either String Cmd
one k [f] = Right (k f)
one _ _ = Left "expected exactly one FILE.sga"

main :: IO ()
main = do
  -- Archive names are UTF-8 regardless of the user's locale, so argv and
  -- extracted file names must be too (ROUNDTRIP keeps undecodable bytes intact).
  mkTextEncoding "UTF-8//ROUNDTRIP" >>= setFileSystemEncoding
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  if args `elem` [[], ["-h"], ["--help"], ["help"]]
    then putStr usage
    else case parseArgs args of
      Left err -> hPutStrLn stderr ("error: " ++ err ++ "\n") >> hPutStr stderr usage >> exitWith (ExitFailure 2)
      Right cmd ->
        run cmd
          `catches` [ Handler (\(e :: SgaError) -> die' (show e))
                    , Handler (\(e :: IOException) -> die' (show e))
                    ]

die' :: String -> IO a
die' msg = hPutStrLn stderr ("error: " ++ msg) >> exitFailure

run :: Cmd -> IO ()
run (Info f) = withArchive f (putStrLn . renderJson 0 . infoJson)
run (Tree f) = withArchive f $ \ar ->
  forM_ (arDrives ar) $ \d -> do
    nodes <- orDie (driveNodes ar d)
    putStrLn (driveAlias d ++ ":" ++ (if null (driveName d) then "" else "  (" ++ driveName d ++ ")"))
    mapM_ putStrLn (renderTree "" nodes)
run (Ls f) = withArchive f $ \ar -> do
  es <- orDie (allEntries ar)
  forM_ es $ \e ->
    putStrLn (pad 12 (show (feSize (entryFile e))) ++ "  " ++ displayPath e)
run (Cat f p) = withArchive f $ \ar -> do
  es <- orDie (allEntries ar)
  let want = intercalate "/" (splitPath p)
      matches e = displayPath e == p || relPath e == want || displayPath e == want
  case filter matches es of
    [] -> die' ("no such file in archive: " ++ p)
    (e : _) -> do
      bytes <- readEntry ar (entryFile e)
      hSetBinaryMode stdout True
      BL.hPut stdout bytes
run (Unpack o) = unpack o

orDie :: Either SgaError a -> IO a
orDie = either (die' . show) pure

relPath :: Entry -> String
relPath = intercalate "/" . entryPath

displayPath :: Entry -> String
displayPath e = entryDrive e ++ ":" ++ relPath e

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

------------------------------------------------------------------------
-- tree

renderTree :: String -> [Node] -> [String]
renderTree prefix nodes = concat (zipWith go [1 :: Int ..] nodes)
  where
    n = length nodes
    go i node =
      let isLast = i == n
          branch = if isLast then "└── " else "├── "
          below = prefix ++ (if isLast then "    " else "│   ")
       in case node of
            Dir name kids -> (prefix ++ branch ++ name ++ "/") : renderTree below kids
            File fe -> [prefix ++ branch ++ feName fe ++ "  (" ++ formatSize (feSize fe) ++ ")"]

formatSize :: Integral a => a -> String
formatSize n
  | n < 1024 = show (toInteger n) ++ " B"
  | otherwise = go (fromIntegral n / 1024 :: Double) ["KiB", "MiB", "GiB", "TiB"]
  where
    go x [u] = fmt x u
    go x (u : us) | x < 1024 = fmt x u | otherwise = go (x / 1024) us
    go x [] = fmt x "?"
    fmt x u = showFFloat (Just 1) x "" ++ " " ++ u

------------------------------------------------------------------------
-- info (JSON)

data J = JS String | JN Integer | JO [(String, J)] | JA [J]

infoJson :: Archive -> J
infoJson ar =
  JO
    [ ("name",            JS (hdrName h))
    , ("version",         JO [("major", JN (toInteger major)), ("minor", JN (toInteger minor))])
    , ("sha_256",         JS (hex (hdrSha256 h)))
    , ("unk_a",           JN (toInteger (tfUnkA ft)))
    , ("unk_b",           JN (toInteger (tfUnkB ft)))
    , ("block_size",      JN (toInteger (tfBlockSize ft)))
    , ("toc",             JO [("pos", JN (toInteger (hdrTocPos h))), ("size", JN (toInteger (hdrTocSize h)))])
    , ("data",            JO [("pos", JN (toInteger (hdrDataPos h))), ("size", JN (toInteger (hdrDataSize h)))])
    , ("folder_count",    JN (toInteger (length (arFolders' ar))))
    , ("file_count",      JN (toInteger (length files)))
    , ("size_on_disk",    JN (sum (map (toInteger . feSize) files)))
    , ("size_in_archive", JN (sum (map (toInteger . fePackedSize) files)))
    , ("encrypted_files", JN (toInteger (length (filter ((/= 0) . feEncryption) files))))
    , ("drives",          JA [JO [("alias", JS (driveAlias d)), ("name", JS (driveName d))] | d <- arDrives ar])
    ]
  where
    h = arHeader ar
    ft = arFooter ar
    (major, minor) = hdrVersion h
    files = arFilesList ar
    arFolders' = foldr (:) [] . arFolders
    hex = concatMap (\b -> let s = showHex b "" in if length s < 2 then '0' : s else s) . BS.unpack

arFilesList :: Archive -> [FileEntry]
arFilesList = foldr (:) [] . arFiles

renderJson :: Int -> J -> String
renderJson _ (JS s) = jsonString s
renderJson _ (JN n) = show n
renderJson _ (JA []) = "[]"
renderJson ind (JA xs) =
  "[\n" ++ intercalate ",\n" [spaces (ind + 2) ++ renderJson (ind + 2) x | x <- xs] ++ "\n" ++ spaces ind ++ "]"
renderJson _ (JO []) = "{}"
renderJson ind (JO kvs) =
  "{\n"
    ++ intercalate ",\n" [spaces (ind + 2) ++ jsonString k ++ ": " ++ renderJson (ind + 2) v | (k, v) <- kvs]
    ++ "\n"
    ++ spaces ind
    ++ "}"

spaces :: Int -> String
spaces n = replicate n ' '

jsonString :: String -> String
jsonString s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c
      | isControl c && ord c < 0x10000 =
          "\\u" ++ [intToDigit ((ord c `div` d) `mod` 16) | d <- [4096, 256, 16, 1]]
      | otherwise = [c]

------------------------------------------------------------------------
-- unpack

-- | Reject anything that could escape the output directory.
safeSegments :: [String] -> Maybe [String]
safeSegments segs
  | any (== "..") parts = Nothing
  | any (any (== '\0')) parts = Nothing
  | null parts = Nothing
  | otherwise = Just (filter (/= ".") parts)
  where
    parts = concatMap splitPath segs

unpack :: UnpackOpts -> IO ()
unpack o = withArchive (uoSrc o) $ \ar -> do
  existsDir <- doesDirectoryExist (uoOut o)
  existsFile <- doesFileExist (uoOut o)
  when ((existsDir || existsFile) && not (uoForce o)) $
    die' ("output path already exists: " ++ uoOut o ++ " (use -f to extract into it anyway)")
  es <- orDie (allEntries ar)
  dirs <- orDie (allDirs ar)
  let isolate = case uoMode o of
        Merge -> False
        Isolate -> True
        Auto -> length (arDrives ar) > 1
      total = length es
      -- Recreate directories first so that empty ones survive too.
      mkdirs = forM_ dirs $ \(alias, path) ->
        forM_ (safeSegments ((if isolate then [alias] else []) ++ path)) $ \segs ->
          createDirectoryIfMissing True (foldl (</>) (uoOut o) segs)
      destFor e =
        safeSegments ((if isolate then [entryDrive e] else []) ++ entryPath e)
      step (ok, bad) (i, e) = case destFor e of
        Nothing -> do
          hPutStrLn stderr ("skipping unsafe path: " ++ displayPath e)
          pure (ok, bad + 1)
        Just segs -> do
          let dest = foldl (</>) (uoOut o) segs
          r <-
            ( Right
                <$> ( do
                        bytes <- readEntry ar (entryFile e)
                        createDirectoryIfMissing True (takeDirectory dest)
                        BL.writeFile dest bytes
                    )
            )
              `catches` [ Handler (\(ex :: SgaError) -> pure (Left (show ex)))
                        , Handler (\(ex :: IOException) -> pure (Left (show ex)))
                        ]
          case r of
            Right () -> do
              when (uoVerbose o) $ hPutStrLn stderr ("[" ++ show i ++ "/" ++ show total ++ "] " ++ dest)
              pure (ok + 1, bad)
            Left why -> do
              hPutStrLn stderr ("failed: " ++ displayPath e ++ ": " ++ why)
              pure (ok, bad + 1)
  mkdirs
  (ok, bad) <- foldM step (0 :: Int, 0 :: Int) (zip [1 :: Int ..] es)
  putStrLn ("extracted " ++ show ok ++ " of " ++ show total ++ " files to " ++ uoOut o)
  unless (bad == 0) $ do
    hPutStrLn stderr (show bad ++ " file(s) could not be extracted")
    exitFailure
