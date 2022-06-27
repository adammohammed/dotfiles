#!/bin/bash
return
function curlv {
    local url="https://api.lindev.local/v4"
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
    echo $(curl -s -H "Authorization: Bearer vagrant" -H "Content-type: application/json" $method "$url/$1" $data_flag "$data")
}

function curld {
    [[ -z "${LINODE_API_DEV_TOKEN}" ]] && load_api_tokens
    local url="https://api.dev.linode.com/v4"
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
    echo $(curl -s -H "Authorization: Bearer $LINODE_API_DEV_TOKEN" -H "Content-type: application/json" $method "$url/$1" $data_flag "$data")
}

function curlp {
    [[ -z "${LINODE_API_TOKEN}" ]] && load_api_tokens
    local url="https://api.linode.com/v4beta"
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
    echo $(curl -s -H "Authorization: Bearer $LINODE_API_TOKEN" -H "Content-type: application/json" $method "$url/$1" $data_flag "$data")
}

function bcurlv {
    local url="https://bapi.lindev.local"
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
    echo $(curl -s --cert ~/devenv/certs/wildcard.crt --key ~/devenv/certs/wildcard.key -H "Content-type: application/json" $method "$url/$1" $data_flag "$data")
}
