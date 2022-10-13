#!/usr/bin/env zsh
# Additions to emacs-vterm-zsh.sh from emacs-libvterm

# Usage:
# find_file file_name_to_search
# - Searches for <file_name_to_search>'
# find_file
# - Opens dired in current directory
vim() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}
# Usage:
# say "This is Major Tom to ground control"
# - "This is Major Tom to ground control"
say() {
    vterm_cmd message "%s" "$*"
}

# This is defined in emacs-libvterm, but is gated behind a check that
# $INSIDE_EMACS is _exactly_ 'vterm' (which does not catch TRAMP vterms).
alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'


# This function allows for opening files, piping into and/or out of emacs.
# - e foo.py just opens foo using emacsclient
# - e -c foo.py creates a new frame
# - e -n foo.py does not!
# myprog.py | e | secondprog.py run the program, load the results into an emacs buffer, then run the saved version through another.
# Source: https://www.reddit.com/r/emacs/comments/fk7p49/piping_stdout_to_emacs/fkrr5i2/

# Allow piping to a buffer
function e () {
    local EMACSOPT=""

    FILES=()
    while [[ $# -gt 0 ]]; do
        key="$1"
        case $key in
            -*) EMACSOPT+="$key "; shift;;
            *) FILES+=("$key"); shift;;
        esac
    done

    # No options given, make our best guess
    if [[ ! "${EMACSOPT}" ]]; then
        if [[ "$INSIDE_EMACS" ]]; then
            EMACSOPT=""
        elif [[ "$DISPLAY" ]]; then
            EMACSOPT="-c "
        else
            EMACSOPT="-nw "
        fi
    fi

    if [ -p /dev/stdin ]; then
        # Input is a pipe, set up temp file
        TMP="$(mktemp)"
        cat > $TMP
        FILES+=("$TMP")
    elif [[ -z "$FILES" && -p /dev/stdout ]]; then
        TMP="$(mktemp)" # piping out needs a temp file
        FILES+=("$TMP")
    fi

    for FILE in "${FILES[@]}"; do
        FILE=$(readlink -m "${FILE}") # Absolute file path
        if [[ -p /dev/stdout || "$EMACSOPT" || "$TMP" ]];then
            emacsclient -q ${EMACSOPT} "$FILE" ;
        else
            if [[ $SSH_CONNECTION ]]; then
                emacsclient -q -e "(find-file-other-window \"/ssh:${ssh_hostname}:$FILE\")";
            else
                emacsclient -q -e "(find-file-other-window \"$FILE\")";
            fi
        fi

        if [ -p /dev/stdout ]; then
            # Output is a pipe
            cat $FILE
        fi
    done

    if [ ! -z "$TMP" ]; then
        # Cleanup tmp files
        rm $TMP
    fi
}
