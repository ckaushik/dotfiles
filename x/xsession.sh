#!/bin/sh

PIPE=~/.xmonad-pipe
rm -f $PIPE
mkfifo -m 600 $PIPE
[ -p $PIPE ] || exit

lxpanel &
cat < $PIPE > /dev/null &
xmonad > $PIPE &
wait $!

wait
