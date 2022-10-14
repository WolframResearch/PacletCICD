#!/bin/bash

wolframscript -script "${1}"
WS_EXIT_CODE=$(echo $?)

if [ $WS_EXIT_CODE -eq 139 ]
then 
    echo "::warning::wolframscript segmentation fault"
    exit 0 
fi
exit $WS_EXIT_CODE