#!/usr/bin/env bash

export PATH=/work/projects/llvm-project/build-release/bin/:$PATH
export LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

desc=$1
tool=$2

# Build the mdl programs. We use the C preprocessor
# to glue together multiple parts of a program
PROG_DIR=`pwd`/clog-programs/juliet
pushd framework
stack run -- --mode gen-prog --desc "$PROG_DIR"
popd

pushd framework
stack run -- --mode magma --desc "$desc" --tool "$tool"
popd
