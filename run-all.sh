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
    local stats_file=$3

    local config_name=`basename $config`
    local clog_program=`jq -r ".clogProgramPath" $config`
    local name=`jq -r ".manifestFilter" $config | sed 's|\.\*||g' | sed 's|CWE|CWE-|g'`
    local output_dir=`jq -r ".outputDir" $config`
    local clang_args=`jq -r ".clangXargs[-1]" $config | sed 's|--checks=-clang-analyzer-\*,||g' | sed 's|--checks=-clang-analyzer\*,||g' | sed 's|clang-analyzer-||g'`
    local n_ground=`grep -c "^.*" $output_dir/ground.csv`
    local n_true_positive_clang=`grep -c "^.*" $output_dir/clang.true.positive.csv`
    local n_false_positive_clang=`grep -c "^.*" $output_dir/clang.false.positive.csv`
    local n_true_positive_clog=`grep -c "^.*" $output_dir/clog.true.positive.csv`
    local n_false_positive_clog=`grep -c "^.*" $output_dir/clog.false.positive.csv`
    local clang_precision=`bc <<< "scale=2; 100.0 * $n_true_positive_clang/($n_true_positive_clang + $n_false_positive_clang)"`
    local clang_recall=`bc <<< "scale=2; 100.0 * $n_true_positive_clang/$n_ground"`
    local clog_precision=`bc <<< "scale=2; 100.0 * $n_true_positive_clog/($n_true_positive_clog + $n_false_positive_clog)"`
    local clog_recall=`bc <<< "scale=2; 100.0 * $n_true_positive_clog/$n_ground"`
    local clang_time=`cat $output_dir/clang.analysis.time`
    local clang_time_f=`printf "%.2f" $clang_time`
    local clog_time=`cat $output_dir/clog.analysis.time`
    local clog_time_f=`printf "%.2f" $clog_time`

    echo "$name & $clang_args & $n_ground & $n_true_positive_clang & $n_true_positive_clog &  $n_false_positive_clang & $n_false_positive_clog & $clang_precision & $clog_precision & $clang_recall & $clog_recall & $clang_time_f & $clog_time_f \\tabularnewline" >> $file

    n_rules=`grep -c ':-' $clog_program`
    n_predicates=`grep -o "[A-Z][A-Za-z_0-9]*(" $clog_program | grep -v "NOT(" | sort -u  | wc -l`
    n_patterns=`grep -c '<:' $clog_program`

    echo "$name & $n_predicates & $n_patterns & $n_rules \\tabularnewline" >> $stats_file
}

RESULT_FILE=juliet-results.tex
STATS_FILE=juliet-stats.tex

echo "" > $RESULT_FILE
echo "" > $STATS_FILE

for config in "${CONFIGS[@]}"; do
    ./run.sh $JULIET/$config
    collect_results $JULIET/$config $RESULT_FILE $STATS_FILE
done
