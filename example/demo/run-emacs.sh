#!/bin/bash
# On macOS, plain `emacs` is often not on $PATH; point EMACS at Emacs.app
# if needed, e.g. EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs.
exec "${EMACS:-emacs}" -Q -nw --load init.el Person.shex
