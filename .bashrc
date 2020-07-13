#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -Alh --color=auto'
alias grep='grep --color=auto'
shopt -s autocd
alias mv='mv -i'
alias rm='rm -i'

# PS1='[\u@\h \W]\$ '

alias config='/usr/bin/git --git-dir=/home/wacken/dotFiles --work-tree=/home/wacken'
set -o vi
