if [[ -z "${OP_SESSION_FILE}" ]]; then
    mkdir -p "${HOME}/.cache/1password"
    export OP_SESSION_FILE="${HOME}/.cache/1password/1psession"
fi

function refresh_1p {
    [[ -f "${OP_SESSION_FILE}" ]] && eval $(cat "${OP_SESSION_FILE}")

    if ! op get account >/dev/null 2>&1 ; then
	op signin linode > "${OP_SESSION_FILE}"
	eval $(cat "${OP_SESSION_FILE}")
    fi
}

refresh_1p


function load_api_tokens {
    refresh_1p
    if [[ -z "$LINODE_API_DEV_TOKEN" ]]; then
	LINODE_API_DEV_TOKEN=$(op get item --fields PAT "Cloud Alpha")
	export LINODE_API_DEV_TOKEN
    fi

    if [[ -z "$LINODE_API_TOKEN" ]]; then
	LINODE_API_TOKEN=$(op get item --fields PAT "Cloud Account")
	export LINODE_API_TOKEN
    fi
}
