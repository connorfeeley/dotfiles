# -*- mode: sh; eval: (sh-set-shell "zsh") -*-

# Check if a command exists
has() {
  which "$@" > /dev/null 2>&1
}

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# export COMPLETION_WAITING_DOTS="true"

# Configure ls colors.
# TODO: sync with color schemes?
# http://geoff.greer.fm/lscolors/
export LSCOLORS='Exfxcxdxbxegedabagacad'
export LS_COLORS='di=1;34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'

# Set up history.
export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTFILE=$ZSH_DATA/history
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"


# -------------------------------------
#  LOADING
# -------------------------------------

# Clone zgenom.
[[ -d "${ZGEN_SRC_DIR}" ]] || {
  git clone https://github.com/jandamm/zgenom.git "${ZGEN_SRC_DIR}"
}

# Load zgenom library.
source "${ZGEN_SRC_DIR}/zgenom.zsh"

# Configure plugins before installing.
source "${ZDOTDIR}/config.zsh"

zgenom saved || source "${ZDOTDIR}/plugins.zsh"

# Load functions early.
source "${ZDOTDIR}/functions.zsh"

source "${ZDOTDIR}/aliases.zsh"
source "${ZDOTDIR}/bindings.zsh"
source "${ZDOTDIR}/color.zsh"
source "${ZDOTDIR}/options.zsh"
source "${ZDOTDIR}/widgets.zsh"


# -------------------------------------
#  CLEANUP + OPTIMIZATION
# -------------------------------------

# Long running processes should return time (in seconds) when they complete.
REPORTTIME=2
TIMEFMT="%U user %S system %P cpu %*Es total"

# Disable Oh-My-ZSH's internal updating -- we handle our own updates.
DISABLE_AUTO_UPDATE=true

# Stop TRAMP (in Emacs) from hanging or term/shell from echoing back commands
# https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/config/zsh/config.zsh#L1-L7
if [[ $TERM == dumb || -n $INSIDE_EMACS ]]; then
  unsetopt zle prompt_cr prompt_subst
  whence -w precmd >/dev/null && unfunction precmd
  whence -w preexec >/dev/null && unfunction preexec
  PS1='$ '
fi

# Login greeting ------------------
if [ "$TERM" = "screen" -a ! "$SHOWED_SCREEN_MESSAGE" = "true" ]; then
  detached_screens=$(screen -list | grep Detached)
  if [ ! -z "$detached_screens" ]; then
    echo "+---------------------------------------+"
    echo "| Detached screens are available:       |"
    echo "$detached_screens"
    echo "+---------------------------------------+"
  fi
fi


# -------------------------------------
#  COMPLETIONS
# -------------------------------------

# Speed up autocomplete, force prefix mapping
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh_cache
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")';

# Load completion configuration.
source "${ZDOTDIR}/completions.zsh"

# Load any custom zsh completions we've installed
# TODO: load from zdotdir/completions

# Dedupe PATH.
# https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
typeset -aU path;


# - - - - - - - - - - - - - - - - - - - -
# Miscellaneous
# - - - - - - - - - - - - - - - - - - - -

export GIT_BRANCH_NAME="$(git symbolic-ref --short -q HEAD 2>/dev/null)"

## Auto-generated by my nix config
source $ZDOTDIR/extra.zshrc

[[ -f "${ZDOTDIR}/config.local" ]] \
  && source "${ZDOTDIR}/config.local"

[[ -f "${ZDOTDIR}/.p10k.zsh" ]] \
  && source "${ZDOTDIR}/.p10k.zsh"
