# Regenerate with: cd example/demo && EMACS=/path/to/Emacs vhs highlight.tape
Output highlight.gif

Set Shell "bash"
Set FontSize 15
Set Width 1300
Set Height 1000
Set Theme "Dracula"
Set Padding 10
Set TypingSpeed 60ms

# Start Emacs (with no file) off-screen so the recording begins once it's
# already running.
Hide
Type "clear"
Enter
Sleep 500ms
Type "./run-emacs.sh"
Enter
Sleep 1500ms
Show

# Open the Person/EXTENDS example, which is missing `PREFIX schema:`.
Ctrl+x
Ctrl+f
Type "example/person-extends.shex"
Enter
Sleep 1500ms

# Flymake flags `schema:name` / `schema:mbox` as undefined prefixes.
Sleep 1500ms

# Open the shexc-ts-mode feature menu (C-c C-c).
Ctrl+c
Ctrl+c
Sleep 2.5s

# Close the menu.
Ctrl+g
Sleep 500ms

# Move point onto the first `schema:name` occurrence.
Down 11
Right 37
Sleep 1s

# Re-open the menu and insert the missing PREFIX from the RDFa map.
Ctrl+c
Ctrl+c
Sleep 1500ms
Type "P"
Sleep 1500ms

# Flymake clears both errors now that `schema:` is declared.
Sleep 1500ms

# Scroll down to the <Person> shapeDecl.
Down 25
# Left 37
Sleep 1s

# Turn on live highlighting of shapes reachable from <Person>.
Ctrl+c
Ctrl+l
Sleep 1500ms

# Open the highlighting submenu ("h") and iterate through each
# reachable-shape toggle.  Unlike a plain menu entry, this submenu is
# `:transient t' -- it stays open across every keystroke below instead
# of exiting (and needing to be reopened) after each toggle.
Ctrl+c
Ctrl+c
Sleep 1s
Type "h"
Sleep 1500ms
Type "h"
Sleep 1500ms
Type "x"
Sleep 1500ms
Type "e"
Sleep 1500ms
Type "t"
Sleep 1500ms
Type "p"
Sleep 1500ms

# Dismiss the submenu and the whole menu stack in one go.
Type "q"
Sleep 1500ms
