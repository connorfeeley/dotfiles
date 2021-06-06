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
# export LSCOLORS='Exfxcxdxbxegedabagacad'
# export LS_COLORS='di=1;34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'

# Set up history.
export HISTSIZE=290000
export SAVEHIST=290000
export HISTFILE="${ZSH_DATA:-${HOME}}/.zhistory"

export GENCOMPL_FPATH="${ZDOTDIR}/completions"

umask 022


# -------------------------------------
#  LOADING
# -------------------------------------

# Clone zgenom.
[[ -d "${ZGEN_SRC_DIR}" ]] || {
  git clone https://github.com/jandamm/zgenom.git "${ZGEN_SRC_DIR}"
}

# Load zgenom library.
source "${ZGEN_SRC_DIR}/zgenom.zsh"

# Load plugins.
zgenom saved || {

  ZGEN_LOADED=()
  ZGEN_COMPLETIONS=()

  # Tab completions with an fzf frontend.
  zgenom load Aloxaf/fzf-tab

  zgenom oh-my-zsh

  [[ -z "$SSH_CONNECTION" ]] && {
    zgenom load zdharma/fast-syntax-highlighting
  }

  zgenom load zsh-users/zsh-history-substring-search

  zgenom load unixorn/autoupdate-zgenom

  # Colorize command output.
  zgenom load unixorn/warhol.plugin.zsh

  # @unixorn's macOS helpers.
  zgenom load unixorn/tumult.plugin.zsh

  # Don't forget aliases, or else.
  zgenom load djui/alias-tips

  zgenom load unixorn/git-extra-commands
  zgenom load skx/sysadmin-util

  # Aliases for working with current repo on GitHub.
  zgenom load peterhurford/git-it-on.zsh

  # Encrypt some repo files.
  # TODO: use it or lose it
  # zgenom load StackExchange/blackbox

  # Load some oh-my-zsh plugins
  zgenom oh-my-zsh plugins/aws
  zgenom oh-my-zsh plugins/colored-man-pages
  zgenom oh-my-zsh plugins/command-not-found
  zgenom oh-my-zsh plugins/composer
  zgenom oh-my-zsh plugins/direnv
  zgenom oh-my-zsh plugins/dotenv
  zgenom oh-my-zsh plugins/fd
  zgenom oh-my-zsh plugins/git
  zgenom oh-my-zsh plugins/github
  # TODO: prob requires configuration
  # zgenom oh-my-zsh plugins/jira
  zgenom oh-my-zsh plugins/npm
  # zgenom oh-my-zsh plugins/pass
  zgenom oh-my-zsh plugins/pip
  zgenom oh-my-zsh plugins/python
  zgenom oh-my-zsh plugins/rsync
  zgenom oh-my-zsh plugins/screen
  zgenom oh-my-zsh plugins/sudo
  zgenom oh-my-zsh plugins/vagrant
  zgenom oh-my-zsh plugins/wd
  zgenom oh-my-zsh plugins/wp-cli

  if [[ $(uname -a | grep -ci Darwin) == 1 ]]; then
    # Load macOS-specific plugins
    zgenom oh-my-zsh plugins/brew
    zgenom oh-my-zsh plugins/osx
  fi

  # Jump around, faster.
  zgenom load skywind3000/z.lua

  # Like nvm but with a less-memorable name. And maybe it's faster?
  zgenom load dominik-schwabe/zsh-fnm

  zgenom load hlissner/zsh-autopair \
    autopair.zsh

  # TODO: not updated for experimental cli i.e. the one supporting flakes
  # zgenom load spwhitt/nix-zsh-completions

  zgenom load zsh-users/zsh-completions \
    src

  # fzf completion
  # zgenom load junegunn/fzf \
  #   shell

  # zgenom load srijanshetty/docker-zsh

  # Automatic completion generator based on `--help` usage
  # https://github.com/dim-an/cod
  zgenom load dim-an/cod

  # Manually generate completions from a command's `--help` output
  zgenom load RobSis/zsh-completion-generator

  zgenom load zsh-users/zsh-autosuggestions

  zgenom load softmoth/zsh-vim-mode

  zgenom load romkatv/powerlevel10k \
    powerlevel10k

  zgenom save

}

source "${ZDOTDIR}/config.zsh"
if [[ $TERM != dumb ]]; then
  source "${ZDOTDIR}/keybindings.zsh"
  source "${ZDOTDIR}/completion.zsh"
  source "${ZDOTDIR}/functions.zsh"
  source "${ZDOTDIR}/aliases.zsh"
  source "${ZDOTDIR}/fzf-tab.zsh"

  ##
  function _cache {
    command -v "$1" >/dev/null || return 1
    local cache_dir="${XDG_CACHE_HOME}/${SHELL##*/}"
    local cache="${cache_dir}/$1"
    if [[ ! -f $cache || ! -s $cache ]]; then
      echo "Caching $1"
      mkdir -p $cache_dir
      "$@" >$cache
    fi
    source $cache || rm -f $cache
  }

  has fd && {
    export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git 2>/dev/null"
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="fd -t d . $HOME"
  }


  ## Auto-generated by my nix config
  source $ZDOTDIR/extra.zshrc

  autoload -Uz compinit && compinit -u -d $ZSH_CACHE/zcompdump
  autopair-init
  enable-fzf-tab

  [[ -f ~/.zshrc ]] && source ~/.zshrc
fi

# Dedupe PATH.
# https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
typeset -aU path;

[[ -n "${BASE16_THEME}" ]] && {
  zgenom load fnune/base16-fzf "bash/base16-${BASE16_THEME}.config"
}

export GIT_BRANCH_NAME="$(git symbolic-ref --short -q HEAD 2>/dev/null)"

## Auto-generated by my nix config
source $ZDOTDIR/extra.zshrc

[[ -f "${ZDOTDIR}/config.local" ]] \
  && source "${ZDOTDIR}/config.local"

[[ -f "${ZDOTDIR}/.p10k.zsh" ]] \
  && source "${ZDOTDIR}/.p10k.zsh"
