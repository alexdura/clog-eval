#! /usr/bin/env bash

export PATH=/work/projects/llvm-project/build-release/bin/:$PATH
export LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

for desc in "$@"
do
    pushd framework
    stack  run -- --mode juliet --desc "$desc"
    popd
done
