#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -Alh --color=always'
alias grep='grep --color=auto'
alias emacs='emacsclient -nc -s 1'
alias br='br -dp'
shopt -s autocd
alias mv='mv -i'
alias rm='rm -i'

# PS1='[\u@\h \W]\$ '

alias config='/usr/bin/git --git-dir=/home/wacken/.dotFiles --work-tree=/home/wacken'
set -o vi

source /home/wacken/.config/broot/launcher/bash/br
