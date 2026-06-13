# shexc-mode-for-emacs
This is an emacs major mode for editing ShExC documents
originally created as N3-mode by Hugo Haas and Dave Pawson,
then adapted by Eric Prud'hommeaux and Vladimir Alexiev.

Put shexc-mode.el somewhere emacs can find it (e.g. ~/.emacs.d/vendor/),
and add the following to your .emacs file:
```lisp
;;
;; shexc mode
;;

(add-to-list 'load-path "{folder that contains shexc-mode.el}")
(autoload 'shexc-mode "shexc-mode" "Major mode for ShExC (ShEx Compact Syntax) files" t)

;; Turn on font lock when in shexc mode
(add-hook 'shexc-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.shexc" . shexc-mode)
        '("\\.shex" . shexc-mode))
       auto-mode-alist))

;; Replace {path} with the full path to shexc-mode.el on your system.

;; If you want to make it load just a little faster;
;; C-x f shexc-mode.el
;; M-x byte-compile-file shexc-mode.el
```

You can also use `imenu` to make an index of shapes 
and [`imenu-list`](https://github.com/bmag/imenu-list) to see them in a panel on the right.

```lisp
(defun my-goto-function ()
  "Find the definition of the function/term at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym (imenu sym)
      (call-interactively 'imenu))))
      
(autoload 'imenu-list-minor-mode "imenu-list" nil t)
(add-hook 'shexc-mode-hook 'imenu-add-menubar-index)
(add-hook 'shexc-mode-hook 'imenu-list-minor-mode)

(setq
  imenu-list-auto-resize nil ; fhir.shex has long shape names, so auto-resize doesn't work well
  imenu-list-size 0.3
  ;; imenu-list-focus-after-activation t
)

;; https://github.com/bmag/imenu-list/issues/31
(add-hook
 'imenu-list-major-mode-hook
 (defun my-imenu-list-major-mode-hook ()
   (setq-local truncate-lines t)
   (setq-local scroll-conservatively 0)
   (setq-local scroll-down-aggressively 1)
   (setq-local scroll-up-aggressively 1)))

;;; example key bindings
(global-set-key (kbd "C-x C-i") 'my-goto-function)
(global-set-key (kbd "C-x I") 'imenu-list-smart-toggle)

```
Eg here is the index of shapes for [fhir.shex](http://hl7.org/fhir/fhir.shex) shown in the right panel (buffer `*Ilist*`):

![](shex-imenu-list-fhir.png)

## shexc-ts-mode (tree-sitter)

`shexc-ts-mode.el` is a newer, tree-sitter-based
companion mode built on the grammar at
[tree-sitter-shexc](https://github.com/ericprud/tree-sitter-shexc). Compared
to the original regexp-based `shexc-mode`, it adds:

- structure-aware indentation (`indent-region`, `indent-for-tab-command`/`TAB`)
- line and block commenting that knows about ShExC's nesting (`M-;` via `comment-dwim`)
- toggling between `#`-style and `/* */`-style comments (`C-c C-k`, see below)
- an `imenu` index of shape declarations (no extra setup needed; works with `imenu-list` as above)
- "jump to shape definition" / "find references to shape" via `xref`
  (`M-.` / `M-,` / `M-?`), resolving `@<#Shape>`, `EXTENDS @<#Shape>`,
  `&<#Shape>` and `start = @<#Shape>` references to their declarations
- structural defun-nav (`C-M-a` / `C-M-e`) and smart sexp navigation
  (`C-M-f` / `C-M-b`, `C-M-t`, `kill-sexp`, ...) that treat each shape
  declaration, EXTENDS clause, AND/OR/NOT operand and `{ ... }` body as a unit
- a breadcrumb (`<Shape> > predicate > ...`) for `which-function-mode` /
  `add-log-current-defun`
- generic region expansion via `expreg`/`expand-region`, if installed — their
  tree-sitter-based expansion understands shexc's grammar with no
  shexc-specific configuration, but you do need to install one of the
  packages and bind its expand command yourself (see below)
- code folding of `{ ... }` shape bodies via `hs-minor-mode` (`C-c C-f`)
- renaming a shape label everywhere it's used (`C-c C-r`)
- `flymake` diagnostics (enabled automatically) for undefined shape
  references and (as an orientation aid) repeated predicates within one
  group
- live highlighting of the shapes "germane" to the shape at point —
  required/negated/optional via EXTENDS/AND/OR/NOT (`C-c C-h`)
- a `transient` feature menu summarizing all of the above with live
  keybindings (`C-c C-c`)

### Keybindings at a glance

For a live, scrollable summary of these commands and their current
keybindings (which may differ if you've rebound things), press `C-c C-c`
to open the `transient` feature menu, or `C-h m` for the full mode
description.

| Key                 | Command                                  | What it does |
| ------------------- | ----------------------------------------- | ------------ |
| `M-;`               | `comment-dwim`                            | Comment/uncomment (knows both `#` and `/* */` styles) |
| `C-c C-k`           | `shexc-ts-mode-toggle-comment-style`      | Toggle which style new comments use |
| `C-c C-u`           | `shexc-ts-mode-unwrap-shape`              | Splice out a `<predicate> { ... }` wrapper |
| `C-c C-w`           | `shexc-ts-mode-wrap-in-shape`             | Wrap the region in a new `<predicate> { ... }` |
| `C-c C-r`           | `shexc-ts-mode-rename-shape`              | Rename a shape label everywhere it's used |
| `C-c C-f`           | `shexc-ts-mode-toggle-fold`               | Fold/unfold the `{ ... }` body at point |
| `C-c C-h`           | `shexc-ts-mode-highlight-extends-mode`    | Live-highlight shapes germane to point |
| `C-c C-c`           | `shexc-ts-mode-menu`                      | Open the `transient` feature menu |
| `C-M-a` / `C-M-e`   | `beginning/end-of-defun`                  | Previous/next shape declaration |
| `C-M-f` / `C-M-b`   | `forward/backward-sexp`                   | Move across a shape/triple-expression branch |
| `C-M-t`             | `transpose-sexps`                         | Swap two adjacent branches |
| `M-.` / `M-,` / `M-?` | `xref-find-definitions`/`-go-back`/`-find-references` | Jump to/from/find refs to a shape |

### Setup

You'll need Emacs 29+ (built with tree-sitter support) and a compiled copy of
the `tree-sitter-shexc` grammar:

```sh
git clone https://github.com/ericprud/tree-sitter-shexc
tree-sitter build -o ~/.emacs.d/tree-sitter/libtree-sitter-shexc.dylib tree-sitter-shexc
```

(On Linux, the output filename would be `libtree-sitter-shexc.so`.)

Then in your init file:

```lisp
(add-to-list 'load-path "{folder that contains shexc-ts-mode.el}")
(require 'shexc-ts-mode)

;; tell Emacs where to find the compiled grammar
(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))

(add-to-list 'auto-mode-alist '("\\.shexc?\\'" . shexc-ts-mode))
```

### Comments: `#` line comments and `/* ... */` block comments

ShExC supports two comment styles — `#` runs to the end of the line, and
`/* ... */` can span multiple lines:

```shexc
# a line comment
ex:S {
  ex:p .  # trailing comment
  /* a block comment
     spanning several lines */
  ex:q .
}
```

`shexc-ts-mode` wires both styles into the standard Emacs comment commands
(`comment-dwim`/`comment-region`/`uncomment-region`, all reachable via `M-;`),
so `M-;` comments and uncomments lines/regions the usual way. By default,
*new* comments that `comment-dwim`/`comment-region` insert use `#`, matching
the prevailing convention in ShExC documents.

If you'd rather have new comments use `/* ... */` (handy when commenting out
a multi-line block, since `comment-region` will wrap the whole region in a
single `/* ... */` rather than prefixing every line with `#`), toggle the
style with:

- `M-x shexc-ts-mode-toggle-comment-style`, bound to `C-c C-k`

This only changes what *new* comments look like — comments already present in
the buffer, in either style, are still fontified as comments and still
navigable. (`uncomment-region` does look for the *currently selected* style's
delimiters first, though: in `#` style it won't recognize a pre-existing
`/* ... */` comment, while in `/* ... */` style it can still strip a
pre-existing `#` comment. Toggle to match the existing comment's style first
if you need to remove it.)

#### Why not just use `C-c C-k` from `c-ts-mode`?

`c-ts-mode` (and other CC-family tree-sitter modes) bind `C-c C-k` to
`c-ts-mode-toggle-comment-style`, which toggles between `//` and `/* */`. That
binding lives in `c-ts-base-mode-map`, a keymap specific to the C-family modes
— `shexc-ts-mode` derives directly from `prog-mode` and doesn't inherit it, so
`C-c C-k` would be unbound by default. And even if it *were* reachable,
`c-ts-mode-toggle-comment-style` hardcodes `//` as the line-comment prefix,
which isn't valid in ShExC (`#` is). So `shexc-ts-mode` defines its own
`shexc-ts-mode-toggle-comment-style`, following the same `C-c C-k` convention
but switching between the comment styles ShExC actually supports (`#` and
`/* */`).

### Structural editing: `C-c C-u` / `C-c C-w`

`shexc-ts-mode` provides two tree-sitter-aware structural editing commands,
modeled on the "splice"/"unwrap" and "wrap" operations from `paredit` and
`combobulate`, but specialized to ShExC's `<predicate> { ... }` nested-shape
construct.

#### `shexc-ts-mode-unwrap-shape` (`C-c C-u`)

With point anywhere inside (or on the boundary of) a `<predicate> { ... }`
construct, removes the predicate and its braces and re-indents the inner triple
expressions in place ("splice"/"raise"):

```
<#foo> {                         <#foo> {
  <p1> {               C-c C-u     <p11> @<#bar> ;
    <p11> @<#bar> ;   =========>   <p12> @<#bar> ;
    <p12> @<#bar> ;                <p13> @<foo>?
    <p13> @<foo>?                }
  }
}
```

Point can be anywhere within the nested shape — before the predicate (POINT 1
in the diagram below), between the predicate and `{` (POINT 2), right after
`{` (POINT 3), right before `}` (POINT 4), or right after `}` (POINT 5):

```
<#foo> {
  /*1*/ <p1> /*2*/ { /*3*/
    <p11> @<#bar> ;
    <p12> @<#bar>
  /*4*/ } /*5*/
}
```

This only handles the simple case where the nested shape is the predicate's
whole value (no NodeConstraint like `IRI`, no AND/OR/NOT combination). Note
that any cardinality (`*`/`+`/`?`) on the removed triple constraint is
discarded along with its predicate.

#### `shexc-ts-mode-wrap-in-shape` (`C-c C-w`)

The inverse: select a region of one or more triple expression elements, invoke
`C-c C-w`, and supply a predicate at the prompt — the selection is wrapped in a
new `<predicate> { ... }` shape and re-indented:

```
<#foo> {                         <#foo> {
  <p11> @<#bar> ;      C-c C-w     <p1> {
  <p12> @<#bar> ;     =========>     <p11> @<#bar> ;
  <p13> @<foo>?                      <p12> @<#bar> ;
}                                  }
                                   <p13> @<foo>?
                                 }
```

(Here the region covered `<p11>` through `<p12>`, leaving `<p13>` outside the
new wrapper.) The region snaps to whole triple-expression element boundaries,
so a slightly imprecise selection still wraps cleanly. With no active region,
the single element at point is wrapped.

#### `shexc-ts-mode-rename-shape` (`C-c C-r`)

With point on a shape label — either the `<#Old>` of a `shape_expr_decl`, or
any reference to it (`@<#Old>`, `EXTENDS @<#Old>`, `&<#Old>`,
`start = @<#Old>`) — prompts for a new label (defaulting to the old one, so
you can edit it in place) and renames every occurrence in the buffer,
including the declaration:

```
<#Old> { ex:p . }          C-c C-r, "<#New>"     <#New> { ex:p . }
<#Other> @<#Old>          =================>     <#Other> @<#New>
```

Refuses to rename to a label that's already in use, or to a no-op (same
label).

### Navigation

`C-M-u` / `C-M-n` / `C-M-p` (`backward-up-list` / `forward-list` /
`backward-list`) and `show-paren-mode` all work across `{` / `}` (and `(` / `)` / `[` / `]`) as expected.

Note: ShExC shape labels conventionally use fragment-IRI syntax (`<#Shape>`,
`<http://example.org/onto#Class>`), so `#` appears both as a line-comment
starter and inside IRI text. `shexc-ts-mode` uses a `syntax-propertize`
function backed by the parse tree to distinguish the two, ensuring that a
`<#Shape> { ... }` opening line is not mistakenly treated as starting a comment
that swallows the `{`.

#### Structural defun-nav (`C-M-a` / `C-M-e`)

Each `shape_expr_decl` (`<#Shape> { ... }`) counts as a "defun", so
`beginning-of-defun`/`end-of-defun` (and anything built on them, like
`narrow-to-defun` or `which-function-mode`'s scanning) jump from one shape
declaration to the previous/next one, regardless of how deeply nested point
currently is.

#### Smart sexp navigation (`C-M-f` / `C-M-b`, `C-M-t`, `kill-sexp`, ...)

`forward-sexp`/`backward-sexp` treat each "branch" of a shape or triple
expression as one sexp: an `EXTENDS @<#Shape>` clause, an AND/OR operand, a
NOT-negated shape, a `{ ... }` body, a OneOf disjunct (`A | B`), an EachOf
group (`A ; B`), or a single triple constraint (`<predicate> <value> *`).
This means `C-M-f`/`C-M-b` skip over a whole branch at a time, and commands
built on `forward-sexp` — `C-M-t` (`transpose-sexps`, swap two adjacent
branches), `C-M-k`/`kill-sexp`, `mark-sexp`, etc. — operate at the same
granularity. The exact unit depends on how deeply point is nested: at the
very start of `<#Shape>`'s whole expression, `C-M-f` jumps over the entire
expression; one level in, it jumps over just the next operand/clause.

#### Breadcrumb (`which-function-mode` / `add-log-current-defun`)

`shexc-ts-mode` sets `add-log-current-defun-function`, so
`which-function-mode` (and `add-log-current-defun`, `M-x add-change-log-entry`,
etc.) show where point is as `<#Shape> > predicate > predicate ...` — the
label of the enclosing `shape_expr_decl`, followed by the predicate of each
enclosing `triple_constraint` (outermost first), for the common case of a
predicate's value being itself a `{ ... }` shape containing further
predicates. Enable it buffer-locally with `M-x which-function-mode`, or
globally in your init file with `(which-function-mode 1)`.

#### Folding shape bodies (`C-c C-f`)

`shexc-ts-mode` registers a `hideshow` entry, so multi-line `{ ... }` shape
bodies can be folded:

- `C-c C-f` (`shexc-ts-mode-toggle-fold`) folds/unfolds a `{ ... }` body,
  enabling `hs-minor-mode` automatically if needed. Point doesn't need to be
  inside the braces: anywhere in the enclosing shape declaration works,
  including on a `<#Shape> EXTENDS @<#Other>` header line before its
  `{ ... }` — `C-c C-f` then folds that declaration's (first) body.
- The usual `hs-minor-mode` commands also work: `hs-hide-all`/`hs-show-all`
  (fold/unfold every shape body in the buffer), `hs-hide-level`, etc.

As with hideshow generally, a `{ ... }` body that fits on one line isn't
folded — there's nothing to hide, so `C-c C-f` succeeds but the buffer looks
unchanged. For example, in

```shexc
<Item>
         CLOSED { ex:id IRI ; ex:rolls @<RollCode>+ }
```

the `{ ... }` is on one line, so folding it has no visible effect; reformat
it across multiple lines first if you want to fold it.

#### Region expansion (`expreg` / `expand-region`)

[`expreg`](https://github.com/casouri/expreg) (built on `treesit`) and
[`expand-region`](https://github.com/magnars/expand-region.el) are
third-party packages, not bundled with Emacs — `shexc-ts-mode` does not
install or bind either of them. If you install one (e.g. `M-x
package-install RET expreg RET`) and bind its command, e.g.:

```lisp
(global-set-key (kbd "C-=") #'expreg-expand)   ;; or #'er/expand-region
```

then in `shexc-ts-mode`, repeatedly pressing `C-=` grows the region outward
through the parse tree with no shexc-specific configuration needed: triple
constraint → EachOf/OneOf group → `{ ... }` body → AND/OR/NOT operand →
EXTENDS clause → shape declaration, etc. (With `expreg`, `C-+` /
`expreg-contract` shrinks it back again.)

### Extended-shape highlighting

#### `shexc-ts-mode-highlight-extends-mode` (`C-c C-h`)

Toggle live highlighting of every shape "germane" to the `shape_expr_decl`
containing point: itself, plus every shape transitively reachable via
EXTENDS, a sole shape reference (`<#A> @<#B>`), AND, OR, and NOT. The
highlighting updates automatically as point moves between shapes, and clears
when point leaves all shape declarations. This works standalone — no
manifest file needed (see `shex-manifest-browser` integration below for the
manifest-driven variant).

Each reachable shape is classified by the *strength* of its strongest
reaching path, shown with a different label/predicate face:

| Strength   | Meaning | Label face | Predicate face |
| ---------- | ------- | ---------- | --------------- |
| required   | Itself, anything it EXTENDS, or an AND-conjunct/sole reference — every node conforming to the focal shape also conforms to this one | `shexc-ts-mode-extends-label-face` (`highlight` + bold) | `shexc-ts-mode-extends-predicate-face` (`font-lock-warning-face`) |
| negated    | Reachable only inside a NOT — conformance requires *not* conforming to this shape | `shexc-ts-mode-extends-label-face-negated` (`error` + bold) | `shexc-ts-mode-extends-predicate-face-negated` (`error`) |
| optional   | Reachable via only some branches of an OR | `shexc-ts-mode-extends-label-face-optional` (`shadow` + bold) | `shexc-ts-mode-extends-predicate-face-optional` (`shadow`) |

"Required" preempts "negated"/"optional" if a shape is reachable via more
than one path. Set `shexc-ts-mode-highlight-extends-include-predicates` to
`nil` if you only want the ShapeDecl labels highlighted, not their
predicates, or toggle it on the fly with
`shexc-ts-mode-toggle-highlight-extends-predicates` (also in the feature menu,
`C-c C-c P`) — if `shexc-ts-mode-highlight-extends-mode` is on, the overlays
in the current buffer refresh immediately to match.

#### Programmatic API

The underlying function, used by both the standalone minor mode above and by
`shex-manifest-browser` (below), is:

```
shexc-ts-mode-highlight-extended-shapes pos &optional include-predicates
```

Given a buffer position POS inside a `shape_expr_decl`, this highlights every
germane shape as described above and returns the list of their label-text
strings, in discovery order, after clearing any overlays from a previous
call. To remove the overlays programmatically (or interactively), call:

```
shexc-ts-mode-clear-extends-overlays
```

#### `shex-manifest-browser` integration

When `shex-manifest-browser.el` is loaded alongside `shexc-ts-mode`, the
manifest browser automatically calls `shexc-ts-mode-highlight-extended-shapes`
whenever it navigates to a test entry whose `sht:schema` opens in a buffer
that is in `shexc-ts-mode`.  The highlighted shape is the entry's `sht:shape`
IRI — the overlays update as you move between entries, mirroring the existing
`sht:shape`/`sht:focus` point-highlighting.

To also highlight the predicates of extended shapes, set:

```lisp
(setq shex-manifest-browser-highlight-extended-predicates t)
```

### Flymake diagnostics

`shexc-ts-mode` registers a `flymake` backend (`shexc-ts-mode-flymake`) and
turns on `flymake-mode` automatically, reporting these diagnostics:

- **Errors** — a shape reference (`@<#Typo>`, `EXTENDS @<#Typo>`,
  `&<#Typo>`, `start = @<#Typo>`) whose label has no matching
  `shape_expr_decl` in the buffer.
- **Errors** — a prefixed name (`x:p2`, a value-set IRI, a datatype, ...)
  whose prefix has no matching `PREFIX x: <...>` declaration.
- **Errors** — text the tree-sitter parser couldn't make sense of, e.g. the
  `p3` in `ex: p3 . ;` (`ex:` alone is a valid, if unusual, prefixed name
  with an empty local part — naming the namespace IRI itself — so the parser
  treats *it* as the predicate and then can't place `p3`).
- **Notes** — a predicate that appears more than once as a direct element of
  the same EachOf/OneOf-disjunct group, e.g.
  `{ ex:p . ; ex:q . ; ex:p . }`. This isn't an error — ShEx allows repeated
  predicates — but it's often a typo or a missing `|`/cardinality, so it's
  flagged as a `:note` for orientation. (Predicates repeated across
  *different* OneOf disjuncts, e.g. `{ ex:p . } | { ex:p [1] }`, are *not*
  flagged, since each disjunct is checked independently.)

`flymake` checks the buffer on an idle timer (after
`flymake-no-changes-timeout` seconds) and on save, not on every keystroke, so
a diagnostic may take a moment to appear or disappear after an edit; `M-x
flymake-start` forces an immediate check. Flagged text gets an
error/note face (default: a red/yellow underline) — to read the message:

- In a GUI frame with `tooltip-mode` on (the default), mousing over the
  underlined text shows it in a tooltip.
- Anywhere, including in a terminal, move point onto the underlined text and
  either look at the echo area, or use `C-h .`
  (`display-local-help`) to show the message on demand.
- `flymake-goto-next-error`/`-prev-error` jump between diagnostics (printing
  the message as they go), and `flymake-show-buffer-diagnostics` (`C-c ! l`
  with the default `flymake` bindings) lists every diagnostic in the buffer.

If point is on `@<#Foo>` and none of the above show anything, first confirm
`flymake-mode` is on (it should appear in the mode line as `Flymake`) and that
`<#Foo>` really is undeclared — `shexc-ts-mode-rename-shape` (`C-c C-r`)
renames *every* occurrence of a label, including references like `EXTENDS
@<#Foo>`, so after a rename there's nothing left to flag unless some
occurrence was changed by hand afterwards.

### Feature menu (`C-c C-c`)

`C-c C-c` (`shexc-ts-mode-menu`) opens a `transient` (Magit-style) popup
summarizing the commands above, grouped into "Navigate" and "Edit". Each
entry's description includes its *current* keybinding (via
`where-is-internal`), so the menu stays accurate even if you've rebound
`shexc-ts-mode-map` — and doubles as a quick-reference "features at a
glance" view, complementing `C-h m`.

If `shex-manifest-browser` is loaded, the menu also shows a "Manifest
browser" group with a toggle for
`shex-manifest-browser-highlight-extended-predicates` (also settable
directly, see [Extended-shape highlighting](#extended-shape-highlighting)
above). The toggle takes effect the next time `shex-manifest-browser`
highlights an entry.
