#!/usr/bin/env bash
# Compile-time benchmark: fromAnnotatedDbSettings against fromAnnotatedDbSettingsIndexed.
#
# For each N it generates a database of N tables. Table i has two columns and a foreign key to
# table i-1 and to table i/2 (when these differ), so the number of foreign keys grows with N
# (2N-3 for N >= 2). It compiles the module that defines the database once, then compiles two
# one-line modules that derive the Schema, one per path, and reports each compile's wall time and
# peak RSS (GNU time). A small program then checks that both paths give the same Schema.
#
# fromAnnotatedDbSettings needs a deeper type-family reduction stack than GHC's default (200) from
# about 50 tables on, so its module is compiled with -freduction-depth=0. The indexed path compiles
# with the default depth.
#
# Usage, from the repository root (it builds the library first):
#   bench/compile-time/run.sh [N ...]          (default: 50 100 200)
# Environment: GHC_OPTS (default -O1), GENERIC_RTS (RTS options for the generic compile, e.g.
#              -M40g to stop it at a heap limit), TIME (path to GNU time, default: `time` on PATH),
#              LIMIT_SECONDS (per compile, default 7200), OUT (default dist-newstyle/compile-bench).
set -euo pipefail

sizes=("$@")
[ ${#sizes[@]} -gt 0 ] || sizes=(50 100 200)
ghc_opts=${GHC_OPTS:--O1}
time_bin=${TIME:-$(command -v time || true)}
[ -n "$time_bin" ] && "$time_bin" -f '%e' true > /dev/null 2>&1 || { echo "GNU time is required (set TIME)" >&2; exit 2; }
limit=${LIMIT_SECONDS:-7200}
out=${OUT:-dist-newstyle/compile-bench}
mkdir -p "$out"

# GHC reads the library and its dependencies from the environment file cabal writes.
cabal build -v1 --write-ghc-environment-files=always lib:beam-automigrate
env_file=$(ls -t "$PWD"/.ghc.environment.* | head -n 1)

gen_db() {
  local n=$1 f=$2 i
  {
    cat <<'HS'
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
HS
    echo "module Db$n where"
    cat <<'HS'

import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam.AutoMigrate (AnnotatedDatabaseSettings, defaultAnnotatedDbSettings)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema
import GHC.Generics (Generic)
HS
    for ((i = 1; i <= n; i++)); do
      echo
      echo "data T$i f = T$i"
      echo "  { t${i}Id :: Columnar f Int64,"
      printf '    t%dName :: Columnar f Text' "$i"
      if [ "$i" -gt 1 ]; then printf ',\n    t%dPrev :: PrimaryKey T%d f' "$i" $((i - 1)); fi
      if [ "$i" -gt 2 ]; then printf ',\n    t%dHalf :: PrimaryKey T%d f' "$i" $((i / 2)); fi
      echo
      echo "  }"
      echo "  deriving (Generic, Beamable)"
      echo
      echo "instance Table T$i where"
      echo "  data PrimaryKey T$i f = T${i}Key (Columnar f Int64) deriving (Generic, Beamable)"
      echo "  primaryKey = T${i}Key . t${i}Id"
    done
    echo
    echo "data Db f = Db"
    for ((i = 1; i <= n; i++)); do
      if [ "$i" = 1 ]; then printf '  { '; else printf '    '; fi
      printf 'table%d :: f (TableEntity T%d)' "$i" "$i"
      if [ "$i" -lt "$n" ]; then echo ","; else echo; fi
    done
    echo "  }"
    echo "  deriving (Generic, Database be)"
    echo
    echo "annotatedDb :: AnnotatedDatabaseSettings Postgres Db"
    echo "annotatedDb = defaultAnnotatedDbSettings defaultDbSettings"
  } > "$f"
}

gen_schema() {
  local n=$1 path=$2 fun=$3 f=$4
  cat > "$f" <<HS
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Schema${path}$n (schema) where

import Data.Proxy (Proxy (..))
import Database.Beam.AutoMigrate (Schema, $fun)
import Db$n (annotatedDb)

schema :: Schema
schema = $fun annotatedDb (Proxy @'[])
HS
}

gen_check() {
  local n=$1 f=$2
  cat > "$f" <<HS
module Main (main) where

import qualified Data.Map.Strict as M
import Database.Beam.AutoMigrate (Schema (..))
import qualified SchemaGeneric$n as G
import qualified SchemaIndexed$n as I
import System.Exit (exitFailure)

main :: IO ()
main =
  if G.schema == I.schema
    then putStrLn ("N=$n: same Schema, " <> show (M.size (schemaTables I.schema)) <> " tables")
    else putStrLn "N=$n: the two paths give different Schemas" >> exitFailure
HS
}

# Compile one module, print "seconds peak-MB" or "timeout"/"failed".
measure() {
  local log=$1; shift
  local rc=0
  timeout "$limit" "$time_bin" -f 'BENCH %e %M' ghc -package-env "$env_file" $ghc_opts -i"$dir" -odir "$dir" -hidir "$dir" -c "$@" > "$log" 2>&1 || rc=$?
  if [ "$rc" = 124 ]; then echo "timeout"; return; fi
  if [ "$rc" != 0 ]; then echo "failed($rc)"; head -n 30 "$log" >&2; return; fi
  awk '/^BENCH/ { printf "%.1f s, %d MB", $2, $3 / 1024 }' "$log"
}

printf '| N | FKs | Db module | fromAnnotatedDbSettings | fromAnnotatedDbSettingsIndexed |\n|---|---|---|---|---|\n' > "$out/results.md"
for n in "${sizes[@]}"; do
  dir=$out/n$n
  rm -rf "$dir"; mkdir -p "$dir"
  gen_db "$n" "$dir/Db$n.hs"
  gen_schema "$n" Generic fromAnnotatedDbSettings "$dir/SchemaGeneric$n.hs"
  gen_schema "$n" Indexed fromAnnotatedDbSettingsIndexed "$dir/SchemaIndexed$n.hs"
  gen_check "$n" "$dir/Check$n.hs"
  fks=$(( n >= 2 ? 2 * n - 3 : 0 ))
  db=$(measure "$dir/db.log" "$dir/Db$n.hs")
  idx=$(measure "$dir/indexed.log" "$dir/SchemaIndexed$n.hs")
  gen=$(measure "$dir/generic.log" -freduction-depth=0 "$dir/SchemaGeneric$n.hs" ${GENERIC_RTS:++RTS $GENERIC_RTS -RTS})
  echo "| $n | $fks | $db | $gen | $idx |" | tee -a "$out/results.md"
  if [[ $gen != *MB || $idx != *MB ]]; then
    tail -n 5 "$dir"/*.log
    continue
  fi
  ghc -package-env "$env_file" $ghc_opts -i"$dir" -odir "$dir" -hidir "$dir" -freduction-depth=0 "$dir/Check$n.hs" -o "$dir/check" > "$dir/check.log" 2>&1 || { tail -n 20 "$dir/check.log"; exit 1; }
  "$dir/check"
done
echo
cat "$out/results.md"
