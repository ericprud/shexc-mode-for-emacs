#!/bin/bash
# On macOS, plain `emacs` is often not on $PATH; point EMACS at Emacs.app
# if needed, e.g. EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs.
# Pass an init file as $1 to use something other than init.el (e.g.
# init-navigate.el for the navigate.tape recording).
exec "${EMACS:-emacs}" -Q -nw --load "${1:-init.el}"
