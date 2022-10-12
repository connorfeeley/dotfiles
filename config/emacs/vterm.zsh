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
