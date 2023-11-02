#!/usr/bin/env bash

ROOT=`pwd`

./run.sh \
    $ROOT/clog-programs/juliet/uncontrolled_format_string.json \
    $ROOT/clog-programs/juliet/null_dereference.json  \
    $ROOT/clog-programs/juliet/os_command_injection.json \
    $ROOT/clog-programs/juliet/use_after_free.json
