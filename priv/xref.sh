#!/bin/bash

# main
pushd . &> /dev/null

CBE_ENTRIES=$1

echo "Entries=$CBE_ENTRIES"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CBE_DIR="/home/laszlototh/accurev/MiddlewareCI_laszlo/Middleware/erlang/projects/cbe/apps/cbe/ebin"

cd $CBE_DIR

erl -sname ahoj -pa $SCRIPT_DIR -setcookie kuki -eval "xref_analyze:analyze_cbe($CBE_ENTRIES )."

popd &> /dev/null
