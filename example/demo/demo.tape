# Regenerate with: cd example/demo && EMACS=/path/to/Emacs vhs demo.tape
Output demo.gif

Set Shell "bash"
Set FontSize 15
Set Width 1300
Set Height 600
Set Theme "Dracula"
Set Padding 10
Set TypingSpeed 60ms

Hide
Type "clear"
Enter
Sleep 500ms
Show

# Open the Person.shex example, which is missing `PREFIX schema:`.
Type "./run-emacs.sh"
Enter
Sleep 2s

# Flymake flags `schema:name` / `schema:email` as undefined prefixes.
Sleep 1.5s

# Open the shexc-ts-mode feature menu (C-c C-c).
Ctrl+c
Ctrl+c
Sleep 2.5s

# Close the menu.
Ctrl+g
Sleep 500ms

# Move point onto the first `schema:name` occurrence.
Down 4
Right 9
Sleep 1s

# Re-open the menu and insert the missing PREFIX from the RDFa map.
Ctrl+c
Ctrl+c
Sleep 1.5s
Type "P"
Sleep 1.5s

# Flymake clears both errors now that `schema:` is declared.
Sleep 2s
