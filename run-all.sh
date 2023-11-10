#!/usr/bin/env bash

export LC_NUMERIC="en_US.UTF-8"

ROOT=`pwd`
JULIET=$ROOT/clog-programs/juliet

declare -a CONFIGS=("uncontrolled_format_string.json"
                    "null_dereference.json"
                    "os_command_injection.json"
                    "use_after_free.json"
                    "stack_buffer_overflow.json"
                    "heap_buffer_overflow.json"
                    "buffer_overread.json"
                    "buffer_underread.json"
                    "buffer_underwrite.json"
                    "write_what_where.json")

collect_results() {
    local config=$1
    local file=$2
    # local name=`basename $config`
    local name=`jq -r ".manifestFilter" $config | sed 's|\.\*||g' | sed 's|CWE|CWE-|g'`
    local output_dir=`jq -r ".outputDir" $config`
    local clang_args=`jq -r ".clangXargs[-1]" $config | sed 's|--checks=-clang-analyzer-\*,||g' | sed 's|--checks=-clang-analyzer\*,||g' | sed 's|clang-analyzer-||g'`
    local n_ground=`grep -c "^.*" $output_dir/ground.csv`
    local n_true_positive_clang=`grep -c "^.*" $output_dir/clang.true.positive.csv`
    local n_false_positive_clang=`grep -c "^.*" $output_dir/clang.false.positive.csv`
    local n_true_positive_clog=`grep -c "^.*" $output_dir/clog.true.positive.csv`
    local n_false_positive_clog=`grep -c "^.*" $output_dir/clog.false.positive.csv`
    local clang_precision=`bc <<< "scale=0; $n_true_positive_clang/($n_true_positive_clang + $n_false_positive_clang) * 100"`
    local clang_recall=`bc <<< "scale=0; $n_true_positive_clang/$n_ground * 100"`
    local clog_precision=`bc <<< "scale=0; $n_true_positive_clog/($n_true_positive_clog + $n_false_positive_clog) * 100"`
    local clog_recall=`bc <<< "scale=0; $n_true_positive_clog/$n_ground * 100"`
    local clang_time=`cat $output_dir/clang.analysis.time`
    local clang_time_f=`printf "%.2f" $clang_time`
    local clog_time=`cat $output_dir/clog.analysis.time`
    local clog_time_f=`printf "%.2f" $clog_time`

    echo "$name & $clang_args & $n_ground & $n_true_positive_clang & $n_true_positive_clog & $clang_recall & $clog_recall & $n_false_positive_clang & $n_false_positive_clog & $clang_precision & $clog_precision & $clang_time_f & $clog_time_f \\tabularnewline" >> $file
}

RESULT_FILE=juliet-results.tex

echo "" > $RESULT_FILE

for config in "${CONFIGS[@]}"; do
    # ./run.sh $JULIET/$config
    collect_results $JULIET/$config $RESULT_FILE
done
