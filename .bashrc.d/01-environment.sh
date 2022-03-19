if [[ ":$PATH:" != *":$HOME/go/bin:"* ]]; then
    export PATH="$HOME/go/bin:$PATH"
fi

export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"
