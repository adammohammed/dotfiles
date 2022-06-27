if [[ ":$PATH:" != *":$HOME/go/bin:"* ]]; then
    export PATH="$HOME/go/bin:$PATH"
fi

if [[ ":$PATH:" != *":/usr/local/go/bin:"* ]]; then
    export PATH="$PATH:/usr/local/go/bin"
fi
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"
