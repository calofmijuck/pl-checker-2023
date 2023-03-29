#!/bin/bash

TESTCASES=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
TESTDIR="examples"
WRONG="$(tput setaf 1)✗$(tput sgr0)"
CORRECT="$(tput setaf 2)✓$(tput sgr0)"

function result {
    if [ "$1" == 1 ]
    then
        echo "${WRONG}"
    else
        echo "${CORRECT}"
    fi
}

echo '# Compiling...'
if ! make
then
    echo ''
    echo 'Compile fail.'
    exit 1
fi
echo ''

echo '# Testing K-- interpreter'

for TC in "${TESTCASES[@]}"
do
    TFILE="${TESTDIR}/test${TC}"
    printf "Test %s... " "${TC}"
    COMM="./run ${TFILE}.k-- > ${TFILE}.out"
    if [ -f "${TFILE}.in" ]
    then
        COMM="${COMM} < ${TFILE}.in"
    fi

    eval "${COMM}"

    eval "diff ${TFILE}.ans ${TFILE}.out > /dev/null"
    eval "rm ${TFILE}.out"
    result $?
done
