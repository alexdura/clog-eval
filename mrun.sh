#!/usr/bin/env bash

export PATH=/work/projects/llvm-project/build-release/bin/:$PATH
export LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

desc=$1
tool=$2

pushd framework
stack run -- --mode magma --desc "$desc" --tool "$tool"
popd
