# Tool to unpack and inspect Relic SGA files

```
usage: sgaunpack -viewer COMMAND ...

  info   FILE.sga                  archive metadata as JSON
  tree   FILE.sga                  print the directory tree
  ls     FILE.sga                  flat listing: size and path
  cat    FILE.sga PATH             write one file to stdout
  unpack [-m|-i] [-f] [-v] FILE.sga OUTDIR
                                   extract everything

unpack options:
  -m, --merge    write all drives straight into OUTDIR
  -i, --isolate  write each drive to OUTDIR/<alias>/
                 (default: merge if there is one drive, else isolate)
  -f, --force    allow OUTDIR to already exist
  -v, --verbose  print every extracted file
```
