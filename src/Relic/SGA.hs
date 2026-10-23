{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Parser for Relic "Essence" SGA archives, v10" This is the version for AOE4
-- and Company of Heroes 3
--
-- All integers are little endian.
--
-- > 0x000  "_ARCHIVE"           8 bytes
-- > 0x008  major, minor         u16, u16          (10, 0)
-- > 0x00C  archive name         128 bytes, UTF-16LE, NUL padded
-- > 0x08C  toc_pos              u64   absolute offset of the TOC
-- > 0x094  toc_size             u32
-- > 0x098  data_pos             u64   absolute offset of the data block
-- > 0x0A0  data_size            u32
-- > 0x0A4  reserved             u32 (0), u32 (1)
-- > 0x0AC  sha_256              256 bytes
-- > 0x1AC  (end of header)
--
-- The TOC (offsets below are relative to @toc_pos@):
--
-- > 0x00  drive_pos  drive_count  folder_pos  folder_count
-- >       file_pos   file_count   name_pos    name_size        8 x u32
-- > 0x20  unk_a  unk_b  block_size                             3 x u32
--
-- Records: drive = 148 bytes, folder = 20 bytes, file = 30 bytes.
--
-- > drive   alias[64] name[64] folder_start folder_end file_start file_end root_folder
-- > folder  name_off  folder_start folder_end file_start file_end
-- > file    name_off hash_pos data_off(u64) packed_size size verification(u8) flags(u8) crc
--
-- Ranges are half open and index the archive-wide folder / file tables.
-- Folder names are full backslash separated paths; only the last component
-- is the directory's own name. @flags@ holds the storage type in the low
-- nibble (0 = stored, 1 and 2 = zlib) and the encryption type in the high
-- nibble (0 = none, 1 = AES-128). File data lives at @data_pos + data_off@.
module Relic.SGA
  ( -- * Types
    Header (..)
  , TocFooter (..)
  , Drive (..)
  , Folder (..)
  , FileEntry (..)
  , Archive (..)
  , Node (..)
  , Entry (..)
  , SgaError (..)

    -- * Reading
  , withArchive
  , driveNodes
  , allEntries
  , allDirs
  , readEntry

    -- * Helpers
  , feStorageType
  , feEncryption
  , nodeName
  , splitPath
  ) where

import Codec.Compression.Zlib.Internal qualified as Z
import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal qualified as BLI
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word
import System.IO

-- | Any problem with the archive itself.
newtype SgaError = SgaError String

instance Show SgaError where
  show (SgaError s) = s

instance Exception SgaError

data Header = Header
  { hdrName     :: !String
  , hdrVersion  :: !(Word16, Word16)
  , hdrTocPos   :: !Word64
  , hdrTocSize  :: !Word32
  , hdrDataPos  :: !Word64
  , hdrDataSize :: !Word32
  , hdrSha256   :: !BS.ByteString
  }
  deriving (Show)

data TocFooter = TocFooter
  { tfUnkA      :: !Word32
  , tfUnkB      :: !Word32
  , tfBlockSize :: !Word32
  }
  deriving (Show)

data Drive = Drive
  { driveAlias       :: !String
  , driveName        :: !String
  , driveFolderStart :: !Word32
  , driveFolderEnd   :: !Word32
  , driveFileStart   :: !Word32
  , driveFileEnd     :: !Word32
  , driveRoot        :: !Word32
  }
  deriving (Show)

data Folder = Folder
  { folderName      :: !String
  , folderSubStart  :: !Word32
  , folderSubEnd    :: !Word32
  , folderFileStart :: !Word32
  , folderFileEnd   :: !Word32
  }
  deriving (Show)

data FileEntry = FileEntry
  { feName         :: !String
  , feHashPos      :: !Word32
  , feDataOff      :: !Word64
  , fePackedSize   :: !Word32
  , feSize         :: !Word32
  , feVerification :: !Word8
  , feFlags        :: !Word8
  , feCrc          :: !Word32
  }
  deriving (Show)

feStorageType, feEncryption :: FileEntry -> Word8
feStorageType fe = feFlags fe .&. 0x0F
feEncryption fe = feFlags fe `shiftR` 4

data Archive = Archive
  { arHandle   :: Handle
  , arFileSize :: Integer
  , arHeader   :: Header
  , arFooter   :: TocFooter
  , arDrives   :: [Drive]
  , arFolders  :: Array Int Folder
  , arFiles    :: Array Int FileEntry
  }

-- | A directory tree node.
data Node
  = Dir String [Node]
  | File FileEntry

nodeName :: Node -> String
nodeName (Dir n _) = n
nodeName (File f) = feName f

-- | A file together with where it lives.
data Entry = Entry
  { entryDrive :: String
  , entryPath  :: [String]
  -- ^ directory components followed by the file name
  , entryFile  :: FileEntry
  }

------------------------------------------------------------------------
-- Opening

headerSize :: Int
headerSize = 0x1AC

withArchive :: FilePath -> (Archive -> IO a) -> IO a
withArchive path act = withBinaryFile path ReadMode $ \h -> openArchive h >>= act

openArchive :: Handle -> IO Archive
openArchive h = do
  fsize <- hFileSize h
  pre <- readAt h fsize 0 12
  (major, minor) <- liftE "header" (runAt 0 pre getPreamble)
  when (major /= 10) $
    throwIO . SgaError $
      "unsupported SGA version "
        ++ show major
        ++ "."
        ++ show minor
        ++ " (this tool only reads version 10)"
  hdrBytes <- readAt h fsize 0 headerSize
  hdr <- liftE "header" (runAt 0 hdrBytes getHeader)
  tocBytes <- readAt h fsize (hdrTocPos hdr) (fromIntegral (hdrTocSize hdr))
  (footer, drives, folders, files) <- liftE "table of contents" (parseToc tocBytes)
  pure
    Archive
      { arHandle = h
      , arFileSize = fsize
      , arHeader = hdr
      , arFooter = footer
      , arDrives = drives
      , arFolders = listArray (0, length folders - 1) folders
      , arFiles = listArray (0, length files - 1) files
      }

liftE :: String -> Either String a -> IO a
liftE what = either (\e -> throwIO (SgaError ("bad " ++ what ++ ": " ++ e))) pure

readAt :: Handle -> Integer -> Word64 -> Int -> IO BS.ByteString
readAt h fsize pos n = do
  when (toInteger pos + toInteger n > fsize) $
    throwIO (SgaError "archive is truncated or contains an out-of-range offset")
  hSeek h AbsoluteSeek (toInteger pos)
  bs <- BS.hGet h n
  unless (BS.length bs == n) $ throwIO (SgaError "unexpected end of file")
  pure bs

------------------------------------------------------------------------
-- Parsing

runAt :: Int -> BS.ByteString -> Get a -> Either String a
runAt off bs g = case runGetOrFail g (BL.fromStrict (BS.drop off bs)) of
  Left (_, _, e)  -> Left e
  Right (_, _, a) -> Right a

getPreamble :: Get (Word16, Word16)
getPreamble = do
  magic <- getByteString 8
  when (magic /= "_ARCHIVE") $ fail "not an SGA archive (bad magic word)"
  (,) <$> getWord16le <*> getWord16le

getHeader :: Get Header
getHeader = do
  version   <- getPreamble
  rawName   <- getByteString 128
  tocPos    <- getWord64le
  tocSize   <- getWord32le
  dataPos   <- getWord64le
  dataSize  <- getWord32le
  _reserved <- getByteString 8
  sha       <- getByteString 256
  pure
    Header
      { hdrName     = T.unpack (T.takeWhile (/= '\0') (TE.decodeUtf16LEWith lenientDecode rawName))
      , hdrVersion  = version
      , hdrTocPos   = tocPos
      , hdrTocSize  = tocSize
      , hdrDataPos  = dataPos
      , hdrDataSize = dataSize
      , hdrSha256   = sha
      }

data TocPtrs = TocPtrs
  { tpDrivePos, tpDriveCount   :: !Word32
  , tpFolderPos, tpFolderCount :: !Word32
  , tpFilePos, tpFileCount     :: !Word32
  , tpNamePos                  :: !Word32
  }

getTocPtrs :: Get TocPtrs
-- The eighth word is the size of the name table; names are NUL terminated
-- and run to the end of the TOC, so it is not needed.
getTocPtrs = TocPtrs <$> w <*> w <*> w <*> w <*> w <*> w <*> w <* w
  where
    w = getWord32le

getFooter :: Get TocFooter
getFooter = TocFooter <$> getWord32le <*> getWord32le <*> getWord32le

decodeName :: BS.ByteString -> String
decodeName = T.unpack . TE.decodeUtf8With lenientDecode . BS.takeWhile (/= 0)

nameAt :: BS.ByteString -> Word32 -> String
nameAt buf off
  | o >= BS.length buf = "<invalid name offset " ++ show off ++ ">"
  | otherwise = decodeName (BS.drop o buf)
  where
    o = fromIntegral off

getN :: Int -> Get a -> Get [a]
getN n g = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = g >>= \x -> go (k - 1 :: Int) (x : acc)

section :: String -> BS.ByteString -> Word32 -> Word32 -> Int -> Get a -> Either String [a]
section what toc pos count size g
  | end > toInteger (BS.length toc) =
      Left (what ++ " table lies outside the TOC")
  | otherwise = runAt (fromIntegral pos) toc (getN (fromIntegral count) g)
  where
    end = toInteger pos + toInteger count * toInteger size

parseToc :: BS.ByteString -> Either String (TocFooter, [Drive], [Folder], [FileEntry])
parseToc toc = do
  (p, footer) <- runAt 0 toc ((,) <$> getTocPtrs <*> getFooter)
  let names = BS.drop (fromIntegral (tpNamePos p)) toc
  drives <- section "drive" toc (tpDrivePos p) (tpDriveCount p) 148 getDrive
  folders <- section "folder" toc (tpFolderPos p) (tpFolderCount p) 20 (getFolder names)
  files <- section "file" toc (tpFilePos p) (tpFileCount p) 30 (getFile names)
  pure (footer, drives, folders, files)

getDrive :: Get Drive
getDrive = do
  alias <- decodeName <$> getByteString 64
  name  <- decodeName <$> getByteString 64
  fs    <- getWord32le
  fe    <- getWord32le
  ls    <- getWord32le
  le    <- getWord32le
  root  <- getWord32le
  pure (Drive alias name fs fe ls le root)

getFolder :: BS.ByteString -> Get Folder
getFolder names = do
  nameOff <- getWord32le
  fs      <- getWord32le
  fe      <- getWord32le
  ls      <- getWord32le
  le      <- getWord32le
  pure (Folder (nameAt names nameOff) fs fe ls le)

getFile :: BS.ByteString -> Get FileEntry
getFile names = do
  nameOff      <- getWord32le
  hashPos      <- getWord32le
  dataOff      <- getWord64le
  packed       <- getWord32le
  size         <- getWord32le
  verification <- getWord8
  flags        <- getWord8
  crc          <- getWord32le
  pure (FileEntry (nameAt names nameOff) hashPos dataOff packed size verification flags crc)

------------------------------------------------------------------------
-- Directory tree

-- | Split on both kinds of path separator, dropping empty components.
splitPath :: String -> [String]
splitPath = filter (not . null) . go
  where
    go s = case break (`elem` ['\\', '/']) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : go rest

leafName :: String -> String
leafName s = case splitPath s of
  [] -> s
  parts -> last parts

maxDepth :: Int
maxDepth = 256

folderAt :: Archive -> Int -> Either SgaError Folder
folderAt ar i
  | i < lo || i > hi = Left (SgaError ("folder index " ++ show i ++ " out of range"))
  | otherwise = Right (arFolders ar ! i)
  where
    (lo, hi) = bounds (arFolders ar)

fileAt :: Archive -> Int -> Either SgaError FileEntry
fileAt ar i
  | i < lo || i > hi = Left (SgaError ("file index " ++ show i ++ " out of range"))
  | otherwise = Right (arFiles ar ! i)
  where
    (lo, hi) = bounds (arFiles ar)

-- | The contents of a drive, directories first, each group sorted by name.
driveNodes :: Archive -> Drive -> Either SgaError [Node]
driveNodes ar d = folderAt ar (fromIntegral (driveRoot d)) >>= contents 0
  where
    contents :: Int -> Folder -> Either SgaError [Node]
    contents depth f
      | depth > maxDepth = Left (SgaError "folders nested too deeply (cyclic archive?)")
      | otherwise = do
          subs <- mapM (sub depth) (range (folderSubStart f) (folderSubEnd f))
          fs <- mapM (fmap File . fileAt ar) (range (folderFileStart f) (folderFileEnd f))
          pure (sortOn key subs ++ sortOn key fs)
    sub depth i = do
      f <- folderAt ar i
      Dir (leafName (folderName f)) <$> contents (depth + 1) f
    key = map toLower . nodeName
    range :: Word32 -> Word32 -> [Int]
    range a b = [fromIntegral a .. fromIntegral b - 1]

-- | Every file in the archive, drive by drive.
allEntries :: Archive -> Either SgaError [Entry]
allEntries ar = concat <$> mapM one (arDrives ar)
  where
    one d = flatten (driveAlias d) [] <$> driveNodes ar d
    flatten alias dir = concatMap $ \n -> case n of
      Dir name kids -> flatten alias (dir ++ [name]) kids
      File fe -> [Entry alias (dir ++ [feName fe]) fe]

-- | Every directory (including empty ones) as @(drive alias, path)@.
allDirs :: Archive -> Either SgaError [(String, [String])]
allDirs ar = concat <$> mapM one (arDrives ar)
  where
    one d = go (driveAlias d) [] <$> driveNodes ar d
    go alias dir = concatMap $ \n -> case n of
      Dir name kids -> (alias, dir ++ [name]) : go alias (dir ++ [name]) kids
      File _ -> []

------------------------------------------------------------------------
-- File data

-- | Read a file's contents, decompressing if necessary.
readEntry :: Archive -> FileEntry -> IO BL.ByteString
readEntry ar fe = do
  when (feEncryption fe /= 0) $
    throwIO (SgaError "file is encrypted (AES-128); no key available")
  raw <-
    readAt
      (arHandle ar)
      (arFileSize ar)
      (hdrDataPos (arHeader ar) + feDataOff fe)
      (fromIntegral (fePackedSize fe))
  out <- case feStorageType fe of
    t
      | t == 0 || fePackedSize fe == feSize fe -> pure (BL.fromStrict raw)
      | t == 1 || t == 2 -> either (throwIO . SgaError . ("zlib: " ++)) pure (inflate raw)
      | otherwise -> throwIO (SgaError ("unknown storage type " ++ show t))
  unless (BL.length out == fromIntegral (feSize fe)) $
    throwIO . SgaError $
      "size mismatch: expected " ++ show (feSize fe) ++ " bytes, got " ++ show (BL.length out)
  pure out

inflate :: BS.ByteString -> Either String BL.ByteString
inflate input =
  Z.foldDecompressStreamWithInput
    (\chunk rest -> BLI.chunk chunk <$> rest)
    (\_leftover -> Right BL.empty)
    (Left . show)
    (Z.decompressST Z.zlibFormat Z.defaultDecompressParams)
    (BL.fromStrict input)
