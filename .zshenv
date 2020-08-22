#!bin/zsh
# Default programs:
export EDITOR="emacs"
export TERMINAL="alacritty"
export BROWSER="brave"
export READER="zathura"

# ~/ Clean-up:
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export XSERVERRC="$XDG_CONFIG_HOME"/X11/xserverrc
export XMONAD_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/xmonad"
export XMONAD_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/xmonad"
export XMONAD_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}/xmonad"
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/config
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export JAVA_OPTIONS="-"

export HISTFILE="$XDG_CACHE_HOME"/zsh/history
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages
#export DOTNET_CLI_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/# Environment variables
