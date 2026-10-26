#!/usr/bin/env bash
# installR.sh -- build R from source into a folder of your own, without root (rig needs root).
# Same configure options as the local Posit build in /opt/R/4.6.1 (system OpenBLAS). Used on the GPSC,
# where the site R is older and compute nodes see only GPFS.
#
# Usage: R/installR.sh <version> <prefix>
#   e.g. R/installR.sh 4.6.1 /gpfs/fs7/nrcan/nrcan_forsite/elm001/R
# Installs to <prefix>/<version>; source and logs go in <prefix>/src.
set -euo pipefail
ver=$1
prefix=$2
src=$prefix/src
mkdir -p "$src"
cd "$src"
[ -d "R-$ver" ] || { curl -sSfLO "https://cloud.r-project.org/src/base/R-${ver%%.*}/R-$ver.tar.gz"; tar xzf "R-$ver.tar.gz"; }
cd "R-$ver"
echo "configure (log: $src/configure-$ver.log)"
./configure --prefix="$prefix/$ver" --enable-R-shlib --enable-memory-profiling --with-x --with-blas --with-lapack \
  LIBnn=lib CFLAGS="-g -O2" > "../configure-$ver.log" 2>&1
grep -A20 "R is now configured" "../configure-$ver.log"
echo "make (log: $src/make-$ver.log)"
make -j16 > "../make-$ver.log" 2>&1 # 16 of the login node's cores: it is shared
make install > "../install-$ver.log" 2>&1
"$prefix/$ver/bin/Rscript" -e 'cat(R.version.string, "\n"); print(extSoftVersion()["BLAS"])'
