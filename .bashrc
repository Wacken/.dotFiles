#
# ~/.bashrc
#

# history
export HISTCONTROL=ignoredups:erasedups           # no duplicate entries
export HISTFILE="$XDG_DATA_HOME/bash/history"
export HISTFILESIZE=1000000
export HISTSIZE=1000000

# vi
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Eternal aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc"

# Prompt
PS1='\[\033[01;31m\][\[\033[01;33m\]\u\[\033[01;32m\]@\[\033[01;34m\]\h \[\033[01;35m\]\W\[\033[01;31m\]]\[\033[01;00m\]$ '

# options
shopt -s autocd # don't need cd, only file name
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases

# better completion
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'
