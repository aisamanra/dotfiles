# .bashrc

# User specific aliases and functions
alias cl=clear
alias la='ls -a'
alias ll='ls -l'
alias lpdup='lpr -o sides=two-sided-long-edge -P hp4200n'
alias ros='command -v mosh 2>&1 1>/dev/null && mosh rosencrantz || ssh rosencrantz'
alias guil='command -v mosh 2>&1 1>/dev/null && mosh guildenstern || ssh guildenstern'
alias evil='EVIL=true emacs'
alias evilmacs='EVIL=true emacs'
alias bigmacs='BIG=true emacs'
alias textmacs='NARROW=true emacs'
alias em='EVIL=true BARE=true emacs'
alias objdump='objdump -M intel'
alias bigterm='urxvt -fn "xft:Inconsolata:pixelsize=20"'
alias hex2raw="tr -d '\\\x' | xxd -r -p"

export EDITOR="emacs -nw"
export BASHFILESIZE=50000
