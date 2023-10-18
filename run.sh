#! /usr/bin/env bash

export PATH=/work/projects/llvm-project/build-release/bin/:$PATH
export LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

PROG_DIR="$PWD/clog-programs/juliet"

# Build the mdl programs. We use the C preprocessor
# to glue together multiple parts of a program
pushd framework
stack run -- --mode gen-prog --desc "$PROG_DIR"
popd

for desc in "$@"
do
    pushd framework
    stack  run -- --mode juliet --desc "$desc"
    popd
done
