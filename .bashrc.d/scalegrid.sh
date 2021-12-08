#!/bin/bash
function auth_scale_grid {
    mkdir -p "${HOME}/.cache/scalegrid"
    SG_COOKIE_FILE=$(mktemp --tmpdir="${HOME}/.cache/scalegrid" sg-coookie.XXXXX)
    export SG_COOKIE_FILE
    local login_json=""
    local totp=""
    local payload=""


    # check if item exists
    if ! op get item "ScaleGrid Staging" >/dev/null; then
	unset SG_COOKIE_FILE
	return 1;
    fi

    login_json=$(op get item --fields username,password "ScaleGrid Staging")
    totp=$(op get totp "ScaleGrid Staging")
    payload=$(echo $login_json | jq "{username: .username, password: .password, inputCode: \"$totp\"}")

    curl https://console.staging.linodedb.net/login -c $SG_COOKIE_FILE -d "${payload}"
}

function sgcurl {
    if [[ -z "${SG_COOKIE_FILE}" ]]; then
	echo "Please run auth_scale_grid first!"
	return 1
    fi


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

    echo $(curl -s -b $SG_COOKIE_FILE $method https://console.staging.linodedb.net/$1 $data_flag "$data")
}
