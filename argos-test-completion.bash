#!/bin/bash

_argos-test_completion()
{
    local cur len
    cur=${COMP_WORDS[COMP_CWORD]}
    ARGS="${COMP_WORDS[@]}"
    
    RES=$(argos complete argos-test --options "$ARGS")
    COMPREPLY=($(compgen -W "$RES" -- "$cur"))
}

complete -F _argos-test_completion argos-test
