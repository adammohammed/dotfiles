return
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
