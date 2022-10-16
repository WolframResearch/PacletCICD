#!/bin/bash

wolframscript -script "Scripts/${1}"
WS_EXIT_CODE=$(echo $?)

if [ $WS_EXIT_CODE -eq 139 ]
then 
    echo "::warning::Warning: wolframscript did not exit cleanly"
    exit 0 
fi
exit $WS_EXIT_CODE