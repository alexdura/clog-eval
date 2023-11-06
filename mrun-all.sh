#!/usr/bin/env bash

ROOT=`pwd`

declare -a TOOL_CONFIGS=("null_dereference.json")
declare -a PROJ_CONFIGS=("libpng"
                         "openssl"
                         "sqlite3"
                        )

for tool in "${TOOL_CONFIGS[@]}"; do
    for proj in "${PROJ_CONFIGS[@]}"; do
        echo "Running checker config ${tool} on ${proj}"
        tool_path="${ROOT}/clog-programs/magma/${tool}"
        proj_path="${ROOT}/clog-programs/magma/${proj}.json"
        proj_fixed_path="${ROOT}/clog-programs/magma/${proj}-fixed.json"
        ./mrun.sh "${proj_path}" "${tool_path}"
        ./mrun.sh "${proj_fixed_path}" "${tool_path}"
    done
done
