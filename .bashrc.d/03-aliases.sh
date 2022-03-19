alias emacs="emacsclient -c"
alias e="emacsclient -t"
alias dc="docker-compose -f ~/devenv/docker-compose.yml"
alias k="kubectl"
alias dotfiles="git --git-dir=$HOME/Documents/dotfiles-work.git --work-tree=$HOME"
complete -F __start_kubectl k
