#/usr/bin/env bash
_argos_completions()
{
  local cur prev

  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}

  case ${COMP_CWORD} in
    1)
      COMPREPLY=($(compgen -W "compile" -- ${cur}))
      ;;
    2)
      case ${prev} in
        compile)
          COMPREPLY=($(compgen -W "--help -h --source -s --target -t" -- ${cur}))
          ;;
      esac
      ;;
    *)
      COMPREPLY=()
      ;;
  esac
}

complete -F _argos_completions argos