# Regenerate with: cd example/demo && EMACS=/path/to/Emacs vhs demo.tape
Output demo.gif

Set Shell "bash"
Set FontSize 15
Set Width 1300
Set Height 1350
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
Sleep 1.5s
Show

# Open the Person/EXTENDS example, which is missing `PREFIX schema:`.
Ctrl+x
Ctrl+f
Type "example/person-extends.shex"
Enter
Sleep 1.5s

# Flymake flags `schema:name` / `schema:mbox` as undefined prefixes.
Sleep 1.5s

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
Sleep 1.5s
Type "P"
Sleep 1.5s

# Flymake clears both errors now that `schema:` is declared.
Sleep 1.5s

# Scroll down to the <Person> shapeDecl.
Down 25
Left 37
Sleep 1s

# Turn on live highlighting of shapes reachable from <Person>.
Ctrl+c
Ctrl+l
Sleep 1.5s

# Iterate through each highlight-reachable toggle, rendering after each.
Ctrl+c
Ctrl+c
Sleep 1s
Type "h"
Sleep 2s

Ctrl+c
Ctrl+c
Sleep 1s
Type "H"
Sleep 2s

Ctrl+c
Ctrl+c
Sleep 1s
Type "x"
Sleep 2s

Ctrl+c
Ctrl+c
Sleep 1s
Type "X"
Sleep 2s

Ctrl+c
Ctrl+c
Sleep 1s
Type "e"
Sleep 2s

Ctrl+c
Ctrl+c
Sleep 1s
Type "E"
Sleep 2s
