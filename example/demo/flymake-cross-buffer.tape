# Regenerate with: cd example/demo && EMACS=/path/to/Emacs vhs flymake-cross-buffer.tape
#
# NOT YET RENDERED/VERIFIED AGAINST A REAL VHS RUN -- `vhs` wasn't
# available in the environment this was written in. Treat timings and
# exact keystroke counts (`Down N`, `Right N`) as a first draft: run it
# once, watch where it drifts from the buffer contents below, and
# adjust before trusting the output. See this repo's other `.tape`
# files (`highlight.tape`/`navigate.tape`) for the established, tested
# pattern this one follows.
Output flymake-cross-buffer.gif
Output flymake-cross-buffer.txt

Set Shell "bash"
Set FontSize 15
Set Width 1300
Set Height 1000
Set Theme "Dracula"
Set Padding 10
Set TypingSpeed 40ms

Hide
Type "clear"
Enter
Sleep 500ms
Type "./run-emacs.sh init-flymake-cross-buffer.el"
Enter
Sleep 1500ms
Show

# Open the test manifest and run its last entry ("Employee cross-buffer
# flymake demo") -- shows schema (left) / data (right) / results
# (bottom), all clean.
Escape
Type "xshex-manifest-browser-open"
Enter
Type "flymake-cross-buffer-manifest.yaml" # ../../tests/tests-manifest.yaml
Enter
#Sleep 1s
#Down 6
Sleep 500ms
Enter
Sleep 2s

# Switch to the schema window and tighten the constraints: `.' ->
# `xsd:string'/`xsd:string'/`xsd:integer'. `xsd:' isn't declared yet.
# Each edit: isearch to the OLD text (point lands at the END of the
# match), `Backspace' the match's own length to erase exactly that
# much, then type the full replacement -- more robust than positioning
# by a `Left N' offset and inserting partway through.
Ctrl+x
Type "o"
Sleep 500ms
Ctrl+S
Type "foaf:givenName . ;"
Enter
Backspace 18
Type "foaf:givenName xsd:string ;"
Sleep 500ms
Ctrl+S
Type "foaf:familyName . ;"
Enter
Backspace 19
Type "foaf:familyName xsd:string ;"
Sleep 500ms
Ctrl+S
Type "co:idNo ."
Enter
Backspace 9
Type "co:idNo xsd:integer"
Sleep 1500ms

# Flymake flags the undefined `xsd:' prefix in the schema -- and,
# automatically (no edit in *data* at all), the data buffer's own
# flymake re-checks too, since the schema it depends on changed.
Sleep 1500ms

# Add the missing PREFIX via its direct binding (`C-c C-p',
# `shexc-ts-mode-insert-prefix' -- NOT the `C-c C-c' menu's own "p",
# which opens the unrelated prefix-*map* submenu instead). Point needs
# to actually be on/adjacent to an `xsd:' occurrence for it to target
# that prefix; back up from the end of "xsd:integer" (just typed) to
# right after the colon.
Left 8
Ctrl+c
Ctrl+p
Sleep 1500ms

# Both schema's own error and (once its recheck runs) data's resulting
# datatype mismatch on `foaf:givenName <#Alice>' should now be visible.
Sleep 2s

# Switch to the data window (to its right) and fix the literal.
Ctrl+Right
Sleep 500ms
Ctrl+S
Type "foaf:givenName <#Alice> ;"
Enter
Backspace 25
Type 'foaf:givenName "Alice" ;'
Sleep 500ms

# Change `co:idNo 1 .' to `co:idNo 1.1 .', introducing a fresh datatype
# mismatch (schema still says `xsd:integer').
Ctrl+S
Type "co:idNo 1 ."
Enter
Backspace 11
Type "co:idNo 1.1 ."
Sleep 1500ms

# Relax the schema to accept either datatype -- data's own flymake
# should clear automatically, without ever touching *data* again.
# Done as a single `M-:' (`eval-expression') that finds/replaces the
# text AND re-selects the schema window itself, rather than switching
# windows first and then sending separate isearch keystrokes: switching
# via `other-window' (cycles all FOUR windows now, order not worth
# depending on), `windmove' (confirmed unreliable: `-right' to reach
# *data* always worked, `-left' back never landed on *schema*), and
# even an `eval-expression'-based `select-window' followed by a
# *separate* `Ctrl+S' isearch (confirmed via a debug probe that
# `(buffer-name)' was already correctly *schema* at that point, yet
# the very next isearch still consistently failed to find text that
# was plainly on screen) were all tried and abandoned.
Escape
Type ":"
Type '(with-current-buffer (get-buffer "*schema: Employee cross-buffer flymake demo*") (goto-char (point-min)) (search-forward "co:idNo xsd:integer") (replace-match "co:idNo (xsd:integer OR xsd:decimal)") (select-window (get-buffer-window (current-buffer))))'
Enter
Sleep 2s
