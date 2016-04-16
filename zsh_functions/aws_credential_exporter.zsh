typeset -r current_dir=`dirname $0`

. ${current_dir}/ini_parser.zsh

# {{{ aws_credential_exporter

function aws_credential_exporter {
  typeset -r section=$1
  typeset -r flag=$2

  if [ -s ${HOME}/.aws/credentials ]; then
    ini_parser "${HOME}/.aws/credentials" $section
    export AWS_ACCESS_KEY_ID=${aws_access_key_id}
    export AWS_SECRET_ACCESS_KEY=${aws_secret_access_key}
    export AWS_DEAFULT_PROFILE=${section}

    if [ "${flag}" = "tf" ]; then
      export TF_VAR_access_key=${aws_access_key_id}
      export TF_VAR_secret_key=${aws_secret_access_key}
    fi

  else
    echo "can't open input file: ${HOME}/.aws/credentials"
  fi

}

# }}}
