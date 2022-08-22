#!/usr/bin/env zsh

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    # For sending magic vterm escape sequences
    vterm_printf(){
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    # Substitute '%s' in vterm buffer name with title
    # Name: HOSTNAME:PWD
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

    # Support clearing the scrollback buffer
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

    ###
    ### Prompt and directory tracking/synchronization
    ###
    # Enable directory tracking: sync working directory between vterm and emacs
    # Enable prompt tracking: make emacs aware of the postition of the end of the prompt
    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
    }
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

    ###
    ### Execute commands in emacs from within vterm
    ###
    # Construct escape sequence to pass messages to emacs
    vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        # ... dance for me, puppets, dance!
        vterm_printf "51;E$vterm_elisp"
    }
    # Usage:
    # find_file file_name_to_search
    # - Searches for <file_name_to_search>'
    # find_file
    # - Opens dired in current directory
    find_file() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }
    # Usage:
    # say "This is Major Tom to ground control"
    # - "This is Major Tom to ground control"
    say() {
        vterm_cmd message "%s" "$*"
    }
fi
