if [[ ":$PATH:" != *":$HOME/go/bin:"* ]]; then
    export PATH="$HOME/go/bin:$PATH"
fi

if [[ ":$PATH:" != *":/usr/local/go/bin:"* ]]; then
    export PATH="$PATH:/usr/local/go/bin"
fi
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"

PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Load up rust env
if [[ -d "$HOME/.cargo" ]]; then
    . "$HOME/.cargo/env"
fi
