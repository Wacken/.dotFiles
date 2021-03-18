#
# ~/.bash_profile
#

export PATH=$PATH:/home/wacken/.config/emacs-doom/bin
export PATH=$PATH:/home/wacken/.local/bin
export PATH=$PATH:/home/wacken/System/bin

# Default programs:
export TERM="xterm-256color"
export EDITOR="emacs"
export TERMINAL="kitty"
export BROWSER="brave"
export READER="okular"
export VISUAL="emacs"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# ~/ Clean-up:
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.

export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc"

export XMONAD_CONFIG_HOME="$XDG_CONFIG_HOME/xmonad"
export XMONAD_DATA_HOME="$XDG_DATA_HOME/xmonad"
export XMONAD_CACHE_HOME="$XDG_CACHE_HOME/xmonad"

export DOOMDIR="$XDG_CONFIG_HOME/doom"

export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export NUGET_PACKAGES="$XDG_CACHE_HOME/NuGetPackages"

export XMODIFIERS="@im=ibus"

[[ -f ~/.bashrc ]] && . ~/.bashrc
