# Regenerate with: cd example/demo && EMACS=/path/to/Emacs vhs navigate.tape
Output navigate.gif

Set Shell "bash"
Set FontSize 15
Set Width 1300
Set Height 1000
Set Theme "Dracula"
Set Padding 10
Set TypingSpeed 40ms

# Start Emacs (with no file) off-screen so the recording begins once it's
# already running. Uses init-navigate.el, which echoes M-./M-, in the echo
# area on every xref jump (see its commentary).
Hide
Type "clear"
Enter
Sleep 500ms
Type "./run-emacs.sh init-navigate.el"
Enter
Sleep 1500ms
Show

# Open the Person/EXTENDS example. Unlike highlight.tape's copy under
# example/demo/example/, this is the complete schema one level up (it has
# `PREFIX schema:', so there's no flymake noise to distract from the
# xref navigation this recording is about).
Ctrl+x
Ctrl+f
Type "../person-extends.shex"
Enter
Sleep 1500ms

# Move point down onto `<Person>' (end-of-buffer, then up 2 lines).
Escape
Type ">"
Up 2
Sleep 500ms

# Move into the middle of `@<Tools>' and follow it with M-. .
Right 41
Sleep 800ms
Escape
Type "."
Sleep 1500ms

# ... onto `@<TGeek>' (now inside <Tools>'s declaration) and follow it.
Right 24
Sleep 800ms
Escape
Type "."
Sleep 1500ms

# ... onto `@<Item>' (inside <TGeek>'s declaration) and follow it.
Right 20
Sleep 800ms
Escape
Type "."
Sleep 1500ms

# ... onto `@<RoleCode>' (inside <Item>'s declaration) and follow it.
# <RoleCode> is a plain NodeKind/value-set shape -- no further shape
# references, so this is the bottom of the chain.
Right 42
Sleep 800ms
Escape
Type "."
Sleep 1500ms

# Pop back up the xref stack with M-, , retracing each jump's starting
# point in reverse, with a quicker 400ms dwell since there's nothing new
# to read at each stop. Four pops land back on the original `@<Tools>'
# inside <Person>, exactly where the descent began.
Escape
Type ","
Sleep 400ms
Escape
Type ","
Sleep 400ms
Escape
Type ","
Sleep 400ms
Escape
Type ","
Sleep 1500ms
