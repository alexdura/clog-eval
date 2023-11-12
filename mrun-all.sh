#!/usr/bin/env bash

# Set proper locale for printf to work
export LC_NUMERIC="en_US.UTF-8"


ROOT=`pwd`

declare -a TOOL_CONFIGS=(
    "null_dereference.json"
    "use_after_free.json"
    "os_command_injection.json"
    "uncontrolled_format_string.json"
)

declare -a PROJ_CONFIGS=(
    "libpng"
    "openssl"
    "sqlite3"
    "libxml2"
)

RESULT_FILE=juliet-results.txt
echo "tool,proj,n_clang,n_clang_fixed,n_clog,n_clog_fixed,clog_time,clang_time" > $RESULT_FILE

for tool in "${TOOL_CONFIGS[@]}"; do
    for proj in "${PROJ_CONFIGS[@]}"; do
        echo "Running checker config ${tool} on ${proj}"
        tool_path="${ROOT}/clog-programs/magma/${tool}"
        proj_path="${ROOT}/clog-programs/magma/${proj}.json"
        proj_fixed_path="${ROOT}/clog-programs/magma/${proj}-fixed.json"
        ./mrun.sh "${proj_path}" "${tool_path}"
        ./mrun.sh "${proj_fixed_path}" "${tool_path}"

        clog_prog_name=`jq -r ".clogProgramPath" ${tool_path}`
        clog_prog_name=`basename $clog_prog_name | sed 's|.mdl||g'`
        output_dir=`jq -r ".outputDir" ${proj_path}`"/$clog_prog_name"
        output_dir_fixed=`jq -r ".outputDir" ${proj_fixed_path}`"/$clog_prog_name"
        n_clang_true_positives=`grep -c "^.*" $output_dir/clang.true.positive.csv`
        n_clang_false_positives=`grep -c "^.*" $output_dir/clang.false.positive.csv`
        n_clog_true_positives=`grep -c "^.*" $output_dir/clog.true.positive.csv`
        n_clog_false_positives=`grep -c "^.*" $output_dir/clog.false.positive.csv`
        clang_time=`cat $output_dir/clang.analysis.time`
        clang_time=`printf "%.2f" $clang_time`
        clog_time=`cat $output_dir/clog.analysis.time`
        clog_time=`printf "%.2f" $clog_time`

        n_clang=$(($n_clang_false_positives + $n_clang_true_positives))
        n_clog=$(($n_clog_false_positives + $n_clog_true_positives))

        n_clang_true_positives_fixed=`grep -c "^.*" $output_dir_fixed/clang.true.positive.csv`
        n_clang_false_positives_fixed=`grep -c "^.*" $output_dir_fixed/clang.false.positive.csv`

        n_clog_true_positives_fixed=`grep -c "^.*" $output_dir_fixed/clog.true.positive.csv`
        n_clog_false_positives_fixed=`grep -c "^.*" $output_dir_fixed/clog.false.positive.csv`

        n_clang_fixed=$(($n_clang_false_positives_fixed + $n_clang_true_positives_fixed))
        n_clog_fixed=$(($n_clog_false_positives_fixed + $n_clog_true_positives_fixed))

        echo "$tool,$proj,$n_clang,$n_clang_fixed,$n_clog,$n_clog_fixed,$clog_time,$clang_time" >> $RESULT_FILE
    done
done
