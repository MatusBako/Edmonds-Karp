#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

input_file=(
    "dual_edges.in"
    "large_node_id.in"
    "large_target_id.in"
    "more_edges.in"
    "no_source.in"
    "no_target.in"
    "no_problem.in"
    "correct1.in"
    "correct2.in"
    "correct3.in"
    "correct4.in"
    "correct1.in"
    "correct2.in"
    "correct3.in"
    "correct4.in"
);

RC=(
    1 1 1 1 1 1 1 
    0 0 0 0 0 0 0 0 
);

ARGS=(
    "" "" "" "" "" "" ""
    "-f" "-f" "-f" "-f" 
    "-v" "-v" "-v" "-v" 
);

OUTPUTS=(
    "" "" "" "" "" "" "" 
    "7" 
    "25" 
    "9" 
    "0"
    "1,2,3,5,6"
    "1,6,2,4"
    "1,2,4,6"
    ""
);

make

if [ ! -f ./ford-fulkerson ]; then
    echo "Binary not found."
    exit 1
fi

if [ ! -d ./TestInputs ]; then
    echo "Directory with input files not found."
    exit 1
fi

for i in `seq 0 $((${#input_file[@]} - 1))`; do
    FILE=${input_file[$i]}
    echo "Testing file $FILE"

    OUTPUT=`./ford-fulkerson ${ARGS[$i]} ./TestInputs/${input_file[$i]}`
    RET_VAL=$?

    if [ "$OUTPUT" = "${OUTPUTS[$i]}" ] && [ $RET_VAL -eq ${RC[$i]} ]; then
        echo -e "${GREEN}[OK]${NC} Output: \"$OUTPUT\"";
    else
        echo -e "${RED}[KO]${NC} Output: \"$OUTPUT\"";
    fi

    echo
done



