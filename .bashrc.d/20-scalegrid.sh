#!/bin/bash
export SG_COOKIE_FILE="${HOME}/.cache/scalegrid/sgsession"

function auth_scale_grid {
    mkdir -p "${HOME}/.cache/scalegrid/"
    local login_json=""
    local totp=""
    local payload=""


    # check if item exists
    refresh_1p
    if ! op get item "ScaleGrid Staging" >/dev/null; then
	unset SG_COOKIE_FILE
	return 1;
    fi

    login_json=$(op get item --fields username,password "ScaleGrid Staging")
    totp=$(op get totp "ScaleGrid Staging")
    payload=$(echo $login_json | jq "{username: .username, password: .password, inputCode: \"$totp\"}")

    echo $(curl https://console.staging.linodedb.net/login -c $SG_COOKIE_FILE -d "${payload}")
}


function sgcurl {
    [[ ! -f "${SG_COOKIE_FILE}" ]] && auth_scale_grid

    local OPTIND o flags
    flags="-s"
    while getopts ":v" o; do
	case "${o}" in
	    v)
		flags="-i"
		;;
	esac
    done
    shift $((OPTIND-1))

    local method=""
    local data_flag=""
    local data=""
    if [[ "$#" -eq 3 ]]; then
	method="-X $2"
	data_flag="--data"
	data="$3"
    elif [[ "$#" -eq 2 ]]; then
	method="-X $2"
    fi

    curl $flags -b $SG_COOKIE_FILE -w "\n" $method -H"Accept: application/json" "https://console.staging.linodedb.net/$1" $data_flag "$data"

}