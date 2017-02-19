#!/bin/bash

## Filter out warnings from the output of the previously called command.
## After reading the file containing regexp filters it constructs a grep command
## then executes this command on the output coming from the command.
## In case after the filtering there are still warnings present returns error,
## otherwise okka.

## Original file can have comments && empty lines
FILTER_FILE=$2
## Filter out comments/empty lines, and store in this file temporarily
FILTER_INPUT=$FILTER_FILE.tmp

CMD_OUTPUT=$1
FILTERED_WARNINGS=filtered_warnings.txt

## Creates the egrep command which will be used for filtering
create_grep_command() {
    RES="egrep -v '("
    export NOT_EMPTY=0
    ## Filter out empty lines and comments (^##.+$)
    ## with the following grep command, and store them in the temp file
    cat $FILTER_FILE | egrep -v '(^[[:space:]]*$|^##.+$)' > $FILTER_INPUT
    while read line; do
        export NOT_EMPTY=1
        ## convert the '|'s to '\|'s in the filter, as that will confuse egrep
        LINE2=`echo $line | sed "s/|/\\\|/g"`
        export RES="$RES$LINE2|"
    done < $FILTER_INPUT
    if [ $NOT_EMPTY == "1" ]; then
        RES="${RES%|}"
    fi
    RES="$RES)' $CMD_OUTPUT"
    if [ $NOT_EMPTY == "1" ]; then
        echo $RES
    else
        echo "cat $CMD_OUTPUT"
    fi
}

## At the end checks if after filtering there are still some warnings remaining
check_empty() {
    if [ "`wc -l $FILTERED_WARNINGS | awk '{print $1}'`" -eq "0" ]; then
        echo 0
    else
        echo 1
    fi
}

## Clean up temporary files
delete_temporaries() {
    rm $CMD_OUTPUT
    rm $FILTER_INPUT
    rm $FILTERED_WARNINGS
}

CMD=`create_grep_command`
echo $CMD
eval ${CMD} > $FILTERED_WARNINGS

printf "\n\nCurrent warnings...\n"
cat $FILTERED_WARNINGS
RESULT=`check_empty`
delete_temporaries
exit $RESULT

