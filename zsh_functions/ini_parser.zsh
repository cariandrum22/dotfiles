# {{{ ini_parser

function ini_parser {

  typeset -r filepath=$1
  typeset -r section=$2

  eval $(sed -e 's/[[:space:]]*\=[[:space:]]*/=/g' \
    -e 's/[;#].*$//' \
    -e 's/[[:space:]]*$//' \
    -e 's/^[[:space:]]*//' \
    -e "s/^\(.*\)=\([^\"']*\)$/\1=\"\2\"/" \
    < $filepath | sed -n -e "/^\[$section\]/,/^\s*\[/{/^[^;].*\=.*/p;}")
}

# }}}
