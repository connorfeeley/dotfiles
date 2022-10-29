###: GENERAL ===================================================================

autoload -Uz add-zsh-hook zmv

compdef nom=nix

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

# Quick and dirty alias to push system flake and doom config
function sys-push() {
  # TODO: make less ugly
  gr $DOTFIELD_DIR $DOOMDIR -- git push
}



nom-rebuild() {
  # TODO: make a lot less ugly
  # Quick and dirty alias to pull system flake and doom, build and activate new system config, sync and build doom config
  local systemType="$([[ "$(uname)" == "Darwin" ]] && echo darwin || echo nixos)"

  gr $DOTFIELD_DIR $DOOMDIR -- git pull --autostash --rebase && \
    pushd $DOTFIELD_DIR && \
    nom build $DOTFIELD_DIR#${systemType}Configurations.$(hostname).config.system.build.toplevel && \
    sudo $DOTFIELD_DIR/result/activate && \
    doom-rebuild-fast && \
    popd
}

function doom-rebuild() {
  doom clean && \
    doom sync -u && \
    doom build -r && \
    doom doctor --pager cat
}

function doom-rebuild-fast() {
  doom sync && \
    doom build -r && \
    doom doctor --pager cat
}
