###: GENERAL ===================================================================

autoload -Uz add-zsh-hook zmv

function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# ls on cd
chpwd_ls() { exa --group-directories-first; }
add-zsh-hook -Uz chpwd chpwd_ls

# start new sessions from most recent dir
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs


###: COLOR =====================================================================

#=====================================
# Print the current shell's color palette.
#
# Outputs:
#   Available colors
#=====================================
function color::palette() {
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}

#=====================================
# Print an escape sequence for a given color code.
#
# Usage:
#   color::print <color-code>
# Parameters:
#   Color code
# Outputs:
#   Escape sequence
#=====================================
function color::print() {
    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}

###: UTILS =====================================================================

#=====================================
# Find all .DS_Store files under the current directory, and prompt to remove them
#=====================================
function dotfield::clean-ds-stores() {
    local TARGET="${1:-$PWD}"
    local FILE_LIST="$(fd --no-ignore --hidden --case-sensitive '.DS_Store' "${TARGET}")"

    # 'rm' on Linux, 'trash' on MacOS
    local RM_TOOL="rm"
    # [[ "$(uname)" == "Darwin" ]] && RM_TOOL="trash -v"
    [[ "$(uname)" == "Darwin" ]] && RM_TOOL="trash -v"

    echo "Files to remove:"
    echo "${FILE_LIST}"
    read -k 1 "?Are you sure? (Y/n) "
    echo
    if [[ "${REPLY}" =~ "^[Yy]$" ]]; then
        echo "Removing files with '${RM_TOOL}'"
        "${RM_TOOL}" < "${FILE_LIST}"
    else
        echo "Replied (${REPLY}), aborting."
    fi
}
