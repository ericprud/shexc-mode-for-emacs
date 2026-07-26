;;; shex-validate.el --- ShEx conformance checking via flymake -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0") (rdf-store "0.1.0") (rdf-turtle "0.1.0") (shexc-shexj "3.0.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `flymake-diagnostic-functions' backend that tags non-conformant
;; RDF nodes directly in a Turtle (data) buffer, by validating it
;; against a separate ShExC schema buffer and a separate ShapeMap
;; buffer through the `emacs-rudof' dynamic module
;; (https://github.com/rudof-project/rudof's `emacs-rudof' crate --
;; not yet released; `M-x shex-validate-setup' walks through finding
;; an already-built copy, or cloning rudof and running `cargo build
;; --release -p emacs-rudof' itself, and pointing
;; `shex-validate-rudof-module-path' at the resulting shared
;; library). Also accepts a module built before that crate's
;; `rudof_emacs' -> `emacs-rudof' rename -- see
;; `shex-validate--upgrade-module-path' and
;; `shex-validate--rudof-fn'.
;;
;; Three buffers, one `flymake-mode': the data buffer is the one this
;; backend actually attaches to (`shex-validate-link-buffers');
;; the schema and ShapeMap buffers are just plain text it reads from,
;; not buffers it adds any backend to themselves. `emacs-rudof' is a
;; *held*, mutable handle (a `user-ptr'), not a one-shot CLI
;; invocation -- so this backend only re-reads the schema/ShapeMap
;; buffers when their own `buffer-chars-modified-tick' has actually
;; changed since last loaded, and always re-reads the data buffer
;; (the one flymake is actually rechecking) every time. See
;; `emacs-rudof''s own README's "Load order matters" section for why
;; schema+data must both be (re)loaded before the ShapeMap.
;;
;; Mapping a validation result's node string back to a position in
;; the data buffer (so flymake has somewhere to put the squiggly line)
;; uses `rdf-turtle-parse-buffer''s POSITIONS argument -- a *second*,
;; purely Emacs-side parse of the same buffer text `rudof-read-data'
;; also parses inside Rust. Deliberately not merged into one parse:
;; keeps this file independent of `emacs-rudof' internals (it only
;; ever sees the same plain node/shape/status/reason strings any other
;; caller of `rudof-validate-shex' would), at the cost of
;; parsing the buffer's Turtle text twice per check -- fine for
;; interactive use, revisit if it ever shows up in profiling.

;;; Code:

(require 'seq)
(require 'flymake)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'tabulated-list)
(require 'shexc-shexj)

;; The `emacs-rudof' dynamic module's own functions (`rudof-new',
;; `rudof-read-shex', etc., or a pre-rename module's `rudof-emacs-new'
;; etc. -- see `shex-validate--rudof-fn') are resolved by name at call
;; time via `funcall'/`intern', never called literally, so no
;; `declare-function' for them is needed or possible here.
(declare-function treesit-buffer-root-node "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function module-load "module.c")
(declare-function buffer-chars-modified-tick "buffer.c")

(defgroup shex-validate nil
  "ShEx conformance checking for `turtle-ts-mode' buffers via flymake."
  :group 'shexc-ts)

(defcustom shex-validate-rudof-module-path nil
  "Path to the built `emacs-rudof' dynamic module
\(`libemacs_rudof.dylib'/`.so'/`.dll', depending on platform), e.g.
\"~/checkouts/rudof-project/rudof/target/release/libemacs_rudof.dylib\".
Required before any `shex-validate' function can run -- but nil (the
default) isn't a dead end: `shex-validate--ensure-module' hunts for a
built module on its own, and `M-x shex-validate-setup' can find,
clone, and build one interactively and set (and optionally save) this
option for you.

A path to the older, pre-rename `librudof_emacs.dylib'/`.so'/`.dll'
also works -- see `shex-validate--upgrade-module-path', which
transparently swaps it for a same-directory `libemacs_rudof' build if
one exists, and `shex-validate--rudof-fn', which falls back to that
older module's own `rudof-emacs-' function prefix if it doesn't."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'shex-validate)

;;; Per-data-buffer state

(defvar-local shex-validate-schema-buffer nil
  "Buffer holding the ShExC schema to validate this (data) buffer's
RDF content against. Set via `shex-validate-link-buffers', not
directly.")

(defvar-local shex-validate-shapemap-buffer nil
  "Buffer holding compact-syntax ShapeMap text (e.g.
\"<node1>@<Shape1>,<node2>@<Shape2>\") naming which node(s) in this
\(data) buffer to validate against which shape(s). Set via
`shex-validate-link-buffers', not directly.")

(defvar-local shex-validate--rudof nil
  "This buffer's held `rudof-new' handle, created lazily on the
first check and reused (mutated in place) on every later one.")

(defvar-local shex-validate--schema-tick nil
  "`buffer-chars-modified-tick' of `shex-validate-schema-buffer'
as of the last time its text was loaded into
`shex-validate--rudof' -- nil means \"never loaded yet\".
Compared on every check so the schema (usually much larger and less
frequently edited than the data buffer) is only re-parsed when it
actually changed.")

(defvar-local shex-validate--shapemap-tick nil
  "Like `shex-validate--schema-tick', for
`shex-validate-shapemap-buffer'.")

;;; Cross-buffer re-check: editing the schema/ShapeMap should refresh
;;; the data buffer's own diagnostics too
;;
;; `flymake-mode's `after-change-functions' member only ever reacts to
;; edits in the buffer flymake is actually attached to -- so without
;; this, editing `shex-validate-schema-buffer'/`-shapemap-buffer'
;; leaves the data buffer's own flymake diagnostics stale (still
;; reporting whatever was true before the edit) until something
;; explicitly rechecks the data buffer itself (an edit there, `M-x
;; flymake-start', or `g' in a `shex-validate-results-mode' buffer).
;; A schema/ShapeMap buffer can be depended on by more than one data
;; buffer (e.g. several `shex-manifest-browser' entries sharing a
;; schema by label), so this is a list, not a single buffer.

(defvar-local shex-validate--dependent-data-buffers nil
  "Data buffers to re-check (via `flymake-start') whenever the current
\(schema or ShapeMap) buffer changes -- see
`shex-validate--register-dependent'/`shex-validate-link-buffers'.")

(declare-function flymake--schedule-timer-maybe "flymake")

(defun shex-validate--notify-dependents (&rest _)
  "`after-change-functions' member for a schema/ShapeMap buffer:
reuses flymake's own idle-timer debounce
\(`flymake--schedule-timer-maybe', keyed off `flymake-no-changes-
timeout' exactly like a normal edit in the data buffer itself would
be) to re-check every live, `flymake-mode' data buffer depending on
this buffer, rather than rechecking on every single keystroke here."
  (dolist (data-buffer shex-validate--dependent-data-buffers)
    (when (and (buffer-live-p data-buffer)
               (buffer-local-value 'flymake-mode data-buffer))
      (with-current-buffer data-buffer
        (flymake--schedule-timer-maybe)))))

(defun shex-validate--register-dependent (buffer data-buffer)
  "Arrange for DATA-BUFFER to be rechecked whenever BUFFER (a schema or
ShapeMap buffer) changes -- see `shex-validate--notify-dependents'."
  (with-current-buffer buffer
    (add-to-list 'shex-validate--dependent-data-buffers data-buffer)
    (add-hook 'after-change-functions #'shex-validate--notify-dependents nil t)))

;;; Module loading

(defvar shex-validate--fn-prefix nil
  "Which Lisp function prefix the loaded `emacs-rudof' dynamic module
actually exposes its functions under: \"rudof-\" for a module built
after the crate's `rudof_emacs' -> `emacs-rudof' rename, or the older
\"rudof-emacs-\" for one built before it. A module only ever exposes
one or the other, and once loaded stays loaded for the rest of the
Emacs session -- so this is resolved at most once (by
`shex-validate--ensure-module'), never per buffer. nil beforehand.")

(defun shex-validate--upgrade-module-path (path)
  "If PATH is the older, pre-rename `librudof_emacs' dynamic module
\(from before the `emacs-rudof' crate's own `rudof_emacs' ->
`emacs-rudof' rename) and a `libemacs_rudof' sibling exists in the
same directory -- e.g. PATH is a stale `shex-validate-rudof-module-path'
left over from before a rebuild -- return that sibling's path instead,
so the newer module is preferred without editing the option by hand.
Otherwise (PATH already names a `libemacs_rudof' module, or no such
sibling exists -- an old build that hasn't been redone since the
rename) returns PATH unchanged; `shex-validate--rudof-fn' below is
what makes loading the old module still work in that case."
  (let* ((dir (or (file-name-directory path) default-directory))
         (base (file-name-nondirectory path)))
    (if (string-prefix-p "librudof_emacs" base)
        (let* ((new-base (concat "libemacs_rudof" (substring base (length "librudof_emacs"))))
               (candidate (expand-file-name new-base dir)))
          (if (file-exists-p candidate)
              (progn
                (message "shex-validate: %s is the pre-rename module name; using %s instead"
                         base new-base)
                candidate)
            path))
      path)))

(defun shex-validate--rudof-fn (name)
  "Resolve NAME (a symbol suffix, e.g. `new'/`read-shex') to whichever
of the current (`rudof-NAME') or pre-rename (`rudof-emacs-NAME')
function symbols the loaded module actually bound, per
`shex-validate--fn-prefix' -- set by `shex-validate--ensure-module',
which must run first."
  (unless shex-validate--fn-prefix
    (error "shex-validate--rudof-fn called before shex-validate--ensure-module"))
  (intern (concat shex-validate--fn-prefix (symbol-name name))))

;;; Module discovery and interactive setup
;;
;; `shex-validate-rudof-module-path' being nil (the out-of-the-box
;; state) used to be a dead end: a bare "set it first" `user-error'.
;; Now `shex-validate--ensure-module' first hunts for an already-built
;; module on its own (`shex-validate--module-candidates') and, failing
;; that, hands off to the `shex-validate-setup' wizard -- which can
;; also clone rudof and build the crate -- when it's safe to prompt.
;; "Safe" matters because `shex-validate--ensure-module' is mostly
;; reached from flymake's *idle-timer* re-checks, where popping a
;; minibuffer prompt at the user (again on every re-check) would be
;; hostile; `this-command' being non-nil is the tell that we're inside
;; an explicit user command (`shex-validate-link-buffers', the
;; `shex-validate-quick' commands, `M-x flymake-start', ...) instead.

(defconst shex-validate--rudof-clone-url
  "https://github.com/rudof-project/rudof"
  "Where `shex-validate-setup' offers to clone rudof from.")

(defconst shex-validate--setup-buffer-name "*shex-validate rudof setup*"
  "Buffer `shex-validate-setup' streams its `git clone'/`cargo build'
subprocess output into.")

(defconst shex-validate--module-basenames
  (cond ((eq system-type 'windows-nt) '("emacs_rudof.dll" "rudof_emacs.dll"))
        ((eq system-type 'darwin) '("libemacs_rudof.dylib" "librudof_emacs.dylib"))
        (t '("libemacs_rudof.so" "librudof_emacs.so")))
  "File names a built `emacs-rudof' module can have on this platform,
most preferable first -- the current crate name, then the pre-rename
`rudof_emacs' one (see `shex-validate--upgrade-module-path'). Windows
has no `lib' prefix because cargo doesn't add one for `cdylib's there.")

(defcustom shex-validate-module-install-directory
  (locate-user-emacs-file "shex/")
  "Where `shex-validate-setup' offers to copy the built `emacs-rudof'
module -- so validation keeps working if the rudof checkout it was
built in later moves, gets `cargo clean'ed, or was only cloned for
this purpose in the first place. Also the first place
`shex-validate--module-candidates' looks."
  :type 'directory
  :group 'shex-validate)

(defun shex-validate--module-search-roots ()
  "Directories worth globbing for a rudof checkout (as `ROOT/rudof' or
`ROOT/*/rudof'): the conventional code-checkout homes that actually
exist on this machine, plus `~' itself."
  (seq-filter #'file-directory-p
              (mapcar #'expand-file-name
                      '("~" "~/checkouts" "~/src" "~/git" "~/code"
                        "~/projects" "~/dev"))))

(defun shex-validate--system-locate (name)
  "Paths this machine's file indexer knows for basename NAME, or nil if
it has none (or there's no indexer): `mdfind' (Spotlight) on macOS,
`plocate'/`locate' on GNU/Linux, Everything's `es' on Windows --
whichever exists, tried in that order. Results may be stale (the
index lags the filesystem), so callers must re-check `file-exists-p'."
  (when-let* ((cmd (cond
                    ((executable-find "mdfind") (list "mdfind" "-name" name))
                    ((executable-find "plocate") (list "plocate" "-b" "-l" "50" name))
                    ((executable-find "locate") (list "locate" "-l" "50" name))
                    ((executable-find "es") (list "es" "-n" "50" name)))))
    (ignore-errors (apply #'process-lines-ignore-status cmd))))

(defun shex-validate--module-preference (path)
  "Sort key for a found module PATH -- smaller is better: a
current-name module beats a pre-rename one (their function sets
differ, and the newer crate is the maintained one), a release build
beats a debug one (same code, tens of times faster), and cargo's
canonical `target/PROFILE/' copy beats its `target/PROFILE/deps/'
hardlink of the same file (identical content and mtime, so nothing
else distinguishes them). Age breaks remaining ties in
`shex-validate--module-candidates', not here."
  (+ (or (seq-position shex-validate--module-basenames
                       (file-name-nondirectory path))
         99)
     (if (string-match-p "/release/" path) 0 10)
     (if (string-match-p "/deps/" path) 1 0)))

(defun shex-validate--module-candidates ()
  "Every built `emacs-rudof' module the cheap searches can find on this
machine, most preferable first (see
`shex-validate--module-preference'; ties broken newest-first):
`shex-validate-module-install-directory' (where a previous
`shex-validate-setup' may have copied one), then `ROOT/rudof' /
`ROOT/*/rudof' checkout globs under
`shex-validate--module-search-roots', then the system file indexer
\(`shex-validate--system-locate')."
  (let (hits)
    (dolist (name shex-validate--module-basenames)
      (push (expand-file-name name shex-validate-module-install-directory)
            hits)
      (dolist (root (shex-validate--module-search-roots))
        (dolist (pattern (list (concat root "/rudof/target/*/" name)
                               (concat root "/*/rudof/target/*/" name)))
          (setq hits (nconc (file-expand-wildcards pattern) hits))))
      (dolist (hit (shex-validate--system-locate name))
        (when (equal (file-name-nondirectory hit) name)
          (push hit hits))))
    (sort (seq-filter #'file-exists-p
                      (delete-dups (mapcar #'expand-file-name hits)))
          (lambda (a b)
            (let ((pa (shex-validate--module-preference a))
                  (pb (shex-validate--module-preference b)))
              (if (/= pa pb)
                  (< pa pb)
                (time-less-p
                 (file-attribute-modification-time (file-attributes b))
                 (file-attribute-modification-time (file-attributes a)))))))))

(defun shex-validate--rudof-checkout-p (dir)
  "Non-nil if DIR is a rudof checkout that can build the module this
package needs -- tested by the presence of the `emacs-rudof' crate's
own `Cargo.toml' (a plain `Cargo.toml' alone would also match any
other Rust project; the crate manifest is what proves `cargo build -p
emacs-rudof' has something to build), at any of the locations the
crate has lived at (`bindings/emacs/' currently; `emacs-rudof/' and
pre-rename `rudof_emacs/' at the top level historically)."
  (seq-some (lambda (manifest)
              (file-exists-p (expand-file-name manifest dir)))
            '("bindings/emacs/Cargo.toml"
              "emacs-rudof/Cargo.toml"
              "rudof_emacs/Cargo.toml")))

(defun shex-validate--setup-built-module (checkout)
  "The most preferable already-built module under CHECKOUT's `target/'
directory (release over debug, current crate name over pre-rename --
`shex-validate--module-preference'), or nil if none is built yet."
  (seq-find #'file-exists-p
            (mapcan (lambda (profile)
                      (mapcar (lambda (name)
                                (expand-file-name
                                 (concat "target/" profile "/" name) checkout))
                              shex-validate--module-basenames))
                    '("release" "debug"))))

(defun shex-validate--setup-retry (why)
  "Offer to restart `shex-validate-setup' (re-picking the module file or
directory) because of WHY, or abort with a `user-error'. Always
returns nil: if the user re-picks, the recursive `shex-validate-setup'
run finishes the whole job itself before this returns, so callers
should treat nil as \"stop, nothing left for this run to do\"."
  (if (y-or-n-p (format "%s.  Pick a different file/directory? " why))
      (progn (shex-validate-setup) nil)
    (user-error "shex-validate setup aborted: %s" why)))

(defun shex-validate--setup-subprocess-buffer ()
  "`shex-validate--setup-buffer-name's buffer, created/displayed and
with point at the end, ready for `git clone'/`cargo build' output to
append to whatever earlier setup output it already holds."
  (let ((buffer (get-buffer-create shex-validate--setup-buffer-name)))
    (with-current-buffer buffer (goto-char (point-max)))
    (display-buffer buffer)
    buffer))

(defun shex-validate--setup-clone (dir)
  "Confirm, then clone `shex-validate--rudof-clone-url' into DIR/rudof
-- via `git', or `gh' if there's no git, or by asking the user to
clone it themselves if there's neither -- and return the verified
checkout directory. Declining the confirmation, or the directory
still not being a rudof checkout afterward, goes through
`shex-validate--setup-retry' (re-pick or abort) and returns nil."
  (let ((checkout (expand-file-name "rudof" dir)))
    (if (not (y-or-n-p (format "Clone %s into %s? "
                               shex-validate--rudof-clone-url checkout)))
        (shex-validate--setup-retry
         (format "%s isn't a rudof checkout and you declined cloning one" dir))
      (let ((buffer (shex-validate--setup-subprocess-buffer)))
        (cond
         ((executable-find "git")
          (message "shex-validate: cloning rudof into %s..." checkout)
          (call-process "git" nil buffer t
                        "clone" shex-validate--rudof-clone-url checkout))
         ((executable-find "gh")
          (message "shex-validate: cloning rudof into %s..." checkout)
          (call-process "gh" nil buffer t
                        "repo" "clone" "rudof-project/rudof" checkout))
         (t
          (read-string
           (format "Neither `git' nor `gh' found.  Clone %s into %s yourself, then press RET: "
                   shex-validate--rudof-clone-url checkout)))))
      (if (shex-validate--rudof-checkout-p checkout)
          checkout
        (shex-validate--setup-retry
         (format "%s still isn't a rudof checkout (no emacs-rudof crate manifest)"
                 checkout))))))

(defun shex-validate--setup-checkout-newer-p (checkout built)
  "Non-nil if the rudof checkout CHECKOUT's latest commit postdates the
BUILT module file -- the \"you pulled/committed since the last `cargo
build'\" signal `shex-validate--setup-build' uses to offer a rebuild.
Only commits count: uncommitted source edits aren't seen. No `git',
or CHECKOUT not being a git checkout at all (a release tarball, say),
counts as nil -- assume the build is current rather than pestering
about a staleness this can't actually detect."
  (when-let* (((executable-find "git"))
              (lines (ignore-errors
                       (process-lines "git" "-C" checkout "log" "-1" "--format=%ct")))
              (commit-time (string-to-number (car lines))))
    (time-less-p (file-attribute-modification-time (file-attributes built))
                 commit-time)))

(defun shex-validate--setup-build (checkout)
  "Get a built module out of the rudof checkout CHECKOUT and hand it to
`shex-validate--setup-finish' -- building first
\(`shex-validate--setup-run-build') when there's no build yet, or when
the user wants a fresh one because CHECKOUT has commits newer than the
existing build (`shex-validate--setup-checkout-newer-p' -- so \"I
pulled rudof, now rebuild\" is just re-running `shex-validate-setup'
and picking the same checkout). Declining the *initial* build
re-picks/aborts (`shex-validate--setup-retry', there's nothing to fall
back on); declining a *re*-build just keeps the existing module."
  (let ((built (shex-validate--setup-built-module checkout)))
    (cond
     ((not built)
      (if (y-or-n-p (format "Run `cargo build --release -p emacs-rudof' in %s (in the background; takes a few minutes)? "
                            checkout))
          (shex-validate--setup-run-build checkout)
        (shex-validate--setup-retry "you declined building the module")))
     ((and (shex-validate--setup-checkout-newer-p checkout built)
           (y-or-n-p (format "%s has commits newer than %s (built %s).  Rebuild? "
                             checkout (file-name-nondirectory built)
                             (format-time-string
                              "%Y-%m-%d %H:%M"
                              (file-attribute-modification-time
                               (file-attributes built))))))
      (shex-validate--setup-run-build checkout))
     (t (shex-validate--setup-finish built)))))

(defun shex-validate--setup-run-build (checkout)
  "Build the emacs-rudof module in CHECKOUT (the caller has already
confirmed the user wants a build): *asynchronously* via `cargo build
--release -p emacs-rudof' (a rudof build takes minutes -- freezing
Emacs on a synchronous one would be worse than the problem being
solved), whose process sentinel then finishes the setup when it
succeeds. If there's no `cargo', asks the user to build it themselves
and verifies afterward, re-picking/aborting via
`shex-validate--setup-retry' if the module still isn't there."
  (if (not (executable-find "cargo"))
      (progn
        (read-string
         (format "No `cargo' found.  Build the module yourself (cd %s && cargo build --release -p emacs-rudof), then press RET: "
                 checkout))
        (if-let* ((built (shex-validate--setup-built-module checkout)))
            (shex-validate--setup-finish built)
          (shex-validate--setup-retry
           (format "still no built module under %s"
                   (expand-file-name "target/" checkout)))))
    (let ((buffer (shex-validate--setup-subprocess-buffer))
          (default-directory checkout))
      (make-process
       :name "shex-validate-rudof-build"
       :buffer buffer
       :command '("cargo" "build" "--release" "-p" "emacs-rudof")
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (if (and (eq (process-status process) 'exit)
                    (zerop (process-exit-status process)))
               (if-let* ((built (shex-validate--setup-built-module checkout)))
                   ;; The remaining setup questions (copy? save?)
                   ;; interrupt whatever the user is doing when the
                   ;; build happens to finish -- tolerable for the
                   ;; one-shot tail of a wizard they launched, and
                   ;; `C-g' here just means "finish later by hand".
                   (condition-case nil
                       (shex-validate--setup-finish built)
                     (quit (message "shex-validate: built %s -- re-run M-x shex-validate-setup to finish pointing at it"
                                    built)))
                 (message "shex-validate: build succeeded but no module found under %s"
                          (expand-file-name "target/" checkout)))
             (message "shex-validate: emacs-rudof build failed -- see %s, then re-run M-x shex-validate-setup"
                      shex-validate--setup-buffer-name)))))
      (message "shex-validate: building emacs-rudof in the background -- see %s"
               shex-validate--setup-buffer-name))))

(defun shex-validate--setup-finish (module)
  "Point `shex-validate-rudof-module-path' at the built MODULE: offer to
copy it into `shex-validate-module-install-directory' first (using it
in place otherwise), then set the option -- via
`customize-save-variable' (persisted for future sessions) or
`customize-set-variable' (this session only), the user's choice."
  (setq module (expand-file-name module))
  (when (y-or-n-p (format "Copy %s into %s (otherwise it's used in place)? "
                          (file-name-nondirectory module)
                          shex-validate-module-install-directory))
    (make-directory shex-validate-module-install-directory t)
    (let ((dest (expand-file-name (file-name-nondirectory module)
                                  shex-validate-module-install-directory)))
      (copy-file module dest t)
      (setq module dest)))
  (if (y-or-n-p "Save `shex-validate-rudof-module-path' for future sessions too? ")
      (customize-save-variable 'shex-validate-rudof-module-path module)
    (customize-set-variable 'shex-validate-rudof-module-path module))
  (message "shex-validate: using %s%s" module
           ;; A dynamic module can't be unloaded, so a path picked
           ;; after one was already loaded only matters on restart.
           (if shex-validate--fn-prefix
               " (the already-loaded module stays in effect until Emacs restarts)"
             "")))

;;;###autoload
(defun shex-validate-setup ()
  "Find -- or clone and build -- the `emacs-rudof' dynamic module and
point `shex-validate-rudof-module-path' at it.

Offers the best already-built module it can find on this machine
\(`shex-validate--module-candidates') as the default; navigating
elsewhere instead can pick either a built module file directly, or a
directory -- one holding a rudof checkout (recognized by the
`emacs-rudof' crate's manifest, `shex-validate--rudof-checkout-p',
directly or as an immediate `rudof/' subdirectory), or one to clone
`shex-validate--rudof-clone-url' into
\(`shex-validate--setup-clone'). Builds the module in the background
if the checkout hasn't already (`shex-validate--setup-build'), then
offers to copy it into `shex-validate-module-install-directory' and to
save the option for future sessions (`shex-validate--setup-finish').

Also entered automatically by `shex-validate--ensure-module' when
validation is first attempted with no module configured or findable --
but only from inside an explicit user command, never from a flymake
idle re-check."
  (interactive)
  (let* ((found (car (shex-validate--module-candidates)))
         (choice (expand-file-name
                  (read-file-name
                   (format "emacs-rudof module%s, or a directory to clone/build rudof in: "
                           (if found (format " (default %s)" found) ""))
                   (and found (file-name-directory found))
                   found t))))
    ;; A directory can be target/release itself, a checkout, a
    ;; checkout's parent, or an empty spot to clone into.
    (let ((direct (and (file-directory-p choice)
                       (seq-find #'file-exists-p
                                 (mapcar (lambda (name)
                                           (expand-file-name name choice))
                                         shex-validate--module-basenames)))))
      (cond
       ((not (file-directory-p choice))
        (shex-validate--setup-finish choice))
       (direct
        (shex-validate--setup-finish direct))
       ((shex-validate--rudof-checkout-p choice)
        (shex-validate--setup-build choice))
       ((shex-validate--rudof-checkout-p (expand-file-name "rudof" choice))
        (shex-validate--setup-build (expand-file-name "rudof" choice)))
       (t
        (when-let* ((checkout (shex-validate--setup-clone choice)))
          (shex-validate--setup-build checkout)))))))

(defun shex-validate--ensure-module ()
  "Load the `emacs-rudof' dynamic module (or a pre-rename
`rudof_emacs' one) if it isn't already, and resolve
`shex-validate--fn-prefix' from whichever it turns out to be.

When `shex-validate-rudof-module-path' is nil -- or names a file that
no longer exists -- first tries to find a built module unattended
\(`shex-validate--module-candidates'); failing that, enters the
interactive `shex-validate-setup' wizard, but only when running inside
an explicit user command (`this-command' non-nil) -- from a flymake
idle-timer re-check (or in batch), it signals a `user-error' naming
the wizard instead of prompting."
  (unless shex-validate--fn-prefix
    (unless (or (fboundp 'rudof-new) (fboundp 'rudof-emacs-new))
      (when (and shex-validate-rudof-module-path
                 (not (file-exists-p
                       (expand-file-name shex-validate-rudof-module-path))))
        (message "shex-validate: configured module %s doesn't exist; looking for another"
                 shex-validate-rudof-module-path)
        (setq shex-validate-rudof-module-path nil))
      (unless shex-validate-rudof-module-path
        (if-let* ((found (car (shex-validate--module-candidates))))
            (progn
              (setq shex-validate-rudof-module-path found)
              (message "shex-validate: using %s (M-x shex-validate-setup to change or save this)"
                       found))
          (if (and this-command (not noninteractive))
              (shex-validate-setup)
            (user-error "No emacs-rudof module found -- run M-x shex-validate-setup"))))
      (unless shex-validate-rudof-module-path
        ;; `shex-validate-setup' returned without setting the path:
        ;; it kicked off a background clone/build.
        (user-error "emacs-rudof is still being set up (background build?) -- retry once it reports success"))
      (module-load
       (shex-validate--upgrade-module-path
        (expand-file-name shex-validate-rudof-module-path))))
    (setq shex-validate--fn-prefix
          (cond
           ((fboundp 'rudof-new) "rudof-")
           ((fboundp 'rudof-emacs-new) "rudof-emacs-")
           (t (error "Loaded %s but neither `rudof-new' nor `rudof-emacs-new' is bound"
                     shex-validate-rudof-module-path))))))

(defun shex-validate--buffer-base-iri (buffer)
  "A \"file://...\" URI for BUFFER's file, or nil if it has none --
passed as ShExC/Turtle's BASE so relative IRIs (including ShExC
`IMPORT') resolve the same way they would for that file on disk."
  (let ((file (buffer-file-name buffer)))
    (and file (concat "file://" file))))

(defun shex-validate--schema-shapemap-base-iri (schema-buffer)
  "The base IRI a ShapeMap's relative shape references should resolve
against, given SCHEMA-BUFFER already loaded via `read-shex' with
`shex-validate--buffer-base-iri' as its own BASE: SCHEMA-BUFFER's own
final `BASE <...>' directive if it declares one -- since that,
not its file location, is then what `read-shex' actually resolved
that schema's own relative shape labels against -- or
`shex-validate--buffer-base-iri' otherwise, matching `read-shex''s
default when SCHEMA-BUFFER declares no BASE of its own."
  (or (with-current-buffer schema-buffer
        (ignore-errors
          (shexc-shexj--ctx-base (shexc-shexj-buffer-directive-ctx))))
      (shex-validate--buffer-base-iri schema-buffer)))

(defun shex-validate--data-shapemap-base-iri (data-buffer)
  "The base IRI a ShapeMap's relative node references should resolve
against, given DATA-BUFFER already loaded via `read-data' with
`shex-validate--buffer-base-iri' as its own BASE: DATA-BUFFER's own
final `BASE <...>' directive if it declares one -- since that, not
its file location, is then what `read-data' actually resolved that
data's own relative subject/object IRIs against -- or
`shex-validate--buffer-base-iri' otherwise, matching `read-data''s
default when DATA-BUFFER declares no BASE of its own.  Mirrors
`shex-validate--schema-shapemap-base-iri' -- confirmed empirically
\(a bare relative node reference in the ShapeMap, e.g. `<#Alice>',
otherwise resolves against `emacs-rudof''s own hidden default
\(the Emacs process's current directory) instead of matching how
DATA-BUFFER's own identical `<#Alice>' reference actually resolved
when `read-data' parsed it, so validation silently can't find that
node in the data at all)."
  (or (with-current-buffer data-buffer
        (ignore-errors
          (rdf-turtle--ctx-base (rdf-turtle-buffer-directive-ctx))))
      (shex-validate--buffer-base-iri data-buffer)))

;;; Running a check

(defvar-local shex-validate-after-check-functions nil
  "Functions called, with the fresh QUADRUPLES result as their sole
argument, every time `shex-validate--check' runs in a data buffer --
not just when `shex-validate-flymake' does, e.g. also when
`shex-manifest-browser' calls `shex-validate--check' directly.  Lets
other code refresh some other cached view of a data buffer's status
\(a results table, node/shape highlights, a summary shown elsewhere\)
on every recheck, including ones `shex-validate--register-dependent'
triggers from an edit in the schema/ShapeMap buffer, not only when the
operator explicitly re-validates.")

(defvar shex-validate--suppress-after-check-hook nil
  "Let-bind to t around a `shex-validate--check' call known to be
redundant with one that just ran in the same buffer (e.g.
`shex-manifest-browser--run' already ran
`shex-validate-after-check-functions' once, explicitly, right before
also calling `shex-validate-show-result-shapemap' -- whose own
`tabulated-list-print' independently re-validates to build its table,
which would otherwise re-trigger the hook a second time for no
reason), so it's skipped instead of running twice.  Also suppresses
`shex-validate-check-error-functions'.")

(defvar-local shex-validate-check-error-functions nil
  "Functions called, with the error object, whenever `shex-validate--
check' itself signals an error (schema/data/ShapeMap failed to parse,
or validation itself errored) -- `shex-validate-after-check-
functions''s counterpart for the case where there's no QUADRUPLES
result to pass it (e.g. so `shex-manifest-browser' can still reflect
\"this entry currently fails\" instead of leaving a stale result
showing).  The error is always re-signalled afterward -- this and
`shex-validate-after-check-functions' are mutually exclusive per call,
never both run.  Does NOT run for the two `user-error's `shex-
validate--check' itself signals directly (no schema/ShapeMap buffer
linked at all) -- those mean nothing is set up yet, not that a setup
that WAS working now has a transient mistake in it, so there's nothing
for a dependent view to usefully reflect.")

(defun shex-validate--check ()
  "Validate the current (data) buffer against
`shex-validate-schema-buffer' per
`shex-validate-shapemap-buffer', returning
`rudof-validate-shex''s result. Signals `user-error' if either
buffer is unset/no longer live."
  (shex-validate--ensure-module)
  (unless (and shex-validate-schema-buffer (buffer-live-p shex-validate-schema-buffer))
    (user-error "No live schema buffer linked -- see `shex-validate-link-buffers'"))
  (unless (and shex-validate-shapemap-buffer (buffer-live-p shex-validate-shapemap-buffer))
    (user-error "No live ShapeMap buffer linked -- see `shex-validate-link-buffers'"))
  (condition-case err
      (shex-validate--check-1)
    (error
     (unless shex-validate--suppress-after-check-hook
       (run-hook-with-args 'shex-validate-check-error-functions err))
     (signal (car err) (cdr err)))))

(defun shex-validate--check-1 ()
  "The body of `shex-validate--check' proper, wrapped there in a
`condition-case' that notifies `shex-validate-check-error-functions'
before re-signalling."
  (unless shex-validate--rudof
    (setq shex-validate--rudof (funcall (shex-validate--rudof-fn 'new))))
  (let ((schema-tick (buffer-chars-modified-tick shex-validate-schema-buffer))
        (shapemap-tick (buffer-chars-modified-tick shex-validate-shapemap-buffer)))
    (unless (equal schema-tick shex-validate--schema-tick)
      (funcall (shex-validate--rudof-fn 'read-shex)
       shex-validate--rudof
       (with-current-buffer shex-validate-schema-buffer (buffer-string))
       nil
       (shex-validate--buffer-base-iri shex-validate-schema-buffer))
      (setq shex-validate--schema-tick schema-tick)
      ;; A changed schema invalidates whatever ShapeMap was loaded
      ;; against the *previous* schema too, even if the ShapeMap
      ;; buffer's own text didn't change -- force its reload below.
      (setq shex-validate--shapemap-tick nil))
    (funcall (shex-validate--rudof-fn 'read-data)
     shex-validate--rudof
     (buffer-string)
     nil
     (shex-validate--buffer-base-iri (current-buffer)))
    (unless (equal shapemap-tick shex-validate--shapemap-tick)
      (funcall (shex-validate--rudof-fn 'read-shapemap)
       shex-validate--rudof
       (with-current-buffer shex-validate-shapemap-buffer (buffer-string))
       nil
       (shex-validate--data-shapemap-base-iri (current-buffer))
       (shex-validate--schema-shapemap-base-iri shex-validate-schema-buffer))
      (setq shex-validate--shapemap-tick shapemap-tick))
    (let ((quadruples (funcall (shex-validate--rudof-fn 'validate-shex) shex-validate--rudof)))
      (unless shex-validate--suppress-after-check-hook
        (run-hook-with-args 'shex-validate-after-check-functions quadruples))
      quadruples)))

(defun shex-validate--check-with-positions (data-buffer)
  "Run `shex-validate--check' in DATA-BUFFER and return
\(QUADRUPLES . POSITIONS) -- POSITIONS is DATA-BUFFER's current
subject-position table (see `rdf-turtle-parse-buffer'), for mapping a
quadruple's node back to a position in DATA-BUFFER. Shared by
`shex-validate-flymake' and
`shex-validate-show-result-shapemap', the two front ends for the
same underlying check."
  (with-current-buffer data-buffer
    (let ((quadruples (shex-validate--check))
          (positions (make-hash-table :test #'equal)))
      (rdf-turtle-parse-buffer (rdf-store-create) positions)
      (cons quadruples positions))))

;;; Diagnostics

(defun shex-validate--diagnostics (quadruples positions)
  "Build `flymake-diagnostic' objects for the current (data) buffer
from QUADRUPLES (`rudof-validate-shex''s result) and POSITIONS
\(`rdf-turtle-parse-buffer''s subject-position table for this same
buffer's current text). \"conformant\"/\"pending\" associations
produce no diagnostic. A node that never occurs as a subject in this
buffer at all (e.g. the ShapeMap names a node this buffer only ever
uses as an object) still gets one diagnostic, at `point-min', so the
failure isn't silently dropped."
  (let (diagnostics)
    (pcase-dolist (`(,node ,shape ,status ,reason) quadruples)
      (unless (member status '("conformant" "pending"))
        (let ((spans (gethash node positions))
              (message (format "ShEx %s against %s: %s" status shape reason)))
          (if spans
              (dolist (span spans)
                (push (flymake-make-diagnostic (current-buffer) (car span) (cdr span) :error message)
                      diagnostics))
            (push (flymake-make-diagnostic
                   (current-buffer) (point-min) (point-max) :error
                   (format "%s (node %s does not occur as a subject in this buffer)" message node))
                  diagnostics)))))
    diagnostics))

;;;###autoload
(defun shex-validate-flymake (report-fn &rest _args)
  "A `flymake-diagnostic-functions' backend: validates the current
buffer's RDF data against `shex-validate-schema-buffer' per
`shex-validate-shapemap-buffer' and reports the result via
REPORT-FN. A setup/configuration failure (module not loaded, buffers
not linked, schema/ShapeMap/data syntax error, ...) is reported as a
single whole-buffer diagnostic rather than left to flymake's own
backend-disabling behavior, so a transient mistake while mid-edit in
the schema buffer doesn't require manually re-enabling this backend
once it's fixed."
  (condition-case err
      (let* ((checked (shex-validate--check-with-positions (current-buffer)))
             (quadruples (car checked))
             (positions (cdr checked)))
        (funcall report-fn (shex-validate--diagnostics quadruples positions)))
    (error
     (funcall report-fn
              (list (flymake-make-diagnostic
                     (current-buffer) (point-min) (point-max) :error
                     (error-message-string err)))))))

;;;###autoload
(defun shex-validate-link-buffers (schema-buffer shapemap-buffer)
  "Link the current (data) buffer to SCHEMA-BUFFER and SHAPEMAP-BUFFER
and enable `shex-validate-flymake' (plus `flymake-mode') in the
current buffer. Call this from the data buffer, once per buffer --
not from the schema/ShapeMap buffers, which get no backend of their
own."
  (interactive
   (list (read-buffer "Schema buffer: " nil t)
         (read-buffer "ShapeMap buffer: " nil t)))
  (setq shex-validate-schema-buffer (get-buffer schema-buffer))
  (setq shex-validate-shapemap-buffer (get-buffer shapemap-buffer))
  (shex-validate--register-dependent shex-validate-schema-buffer (current-buffer))
  (shex-validate--register-dependent shex-validate-shapemap-buffer (current-buffer))
  (add-hook 'flymake-diagnostic-functions #'shex-validate-flymake nil t)
  (flymake-mode 1))

;;; Result ShapeMap buffer
;;
;; Unlike `shex-validate-flymake' (which only ever surfaces
;; failures, in-place), this shows every node/shape association in
;; the ShapeMap -- "conformant" rows included -- in its own
;; `tabulated-list-mode' buffer, one row per association, sortable by
;; any column. `RET' jumps to that row's node in the data buffer; `g'
;; (`revert-buffer', bound by `tabulated-list-mode' already)
;; re-validates and redraws every row in place, exactly like
;; `list-processes'/`package-menu-mode'.

(defvar-local shex-validate-results--data-buffer nil
  "The data buffer this `*ShEx Result ShapeMap*' buffer reports on.")

(defvar-local shex-validate-results--positions nil
  "`shex-validate-results--data-buffer''s subject-position table
\(see `rdf-turtle-parse-buffer'), as of this results buffer's last
\(re)validation -- consulted by `shex-validate-results-goto-node'.")

(defun shex-validate-results--status-face (status)
  (pcase status
    ("conformant" 'success)
    ("pending" 'shadow)
    (_ 'error))) ; "nonconformant"/"inconsistent"

(defvar shex-validate-results--precomputed nil
  "If non-nil, a (QUADRUPLES . POSITIONS) cons -- as
`shex-validate--check-with-positions' returns -- that
`shex-validate-results--entries' uses directly instead of calling
`shex-validate--check-with-positions' (hence `shex-validate--check')
again.  Let-bind this around a `tabulated-list-print' on a results
buffer when fresh results are already on hand (e.g.
`shex-manifest-browser''s own `shex-validate-after-check-functions'
callback), to avoid a redundant revalidation that would also
re-trigger that same hook.")

(defun shex-validate-results--entries (data-buffer)
  "`tabulated-list-entries' for DATA-BUFFER: validate it and return one
entry per `rudof-validate-shex' quadruple, ID'd by `(NODE
. SHAPE)' (a ShapeMap's node/shape associations are themselves a set,
so this is already unique). Refreshes
`shex-validate-results--positions' as a side effect, since this
function is called by `tabulated-list-print' (hence: on every `g').
A transient schema/data/ShapeMap parse failure (`shex-validate--check'
signalling an error, e.g. mid-edit in the schema buffer) produces one
synthetic row showing the error, rather than letting `tabulated-list-
print' itself crash outright with no results shown at all."
  (let ((checked (or shex-validate-results--precomputed
                      (condition-case err
                          (shex-validate--check-with-positions data-buffer)
                        (error (cons :error (error-message-string err)))))))
    (if (eq (car-safe checked) :error)
        (progn
          (setq shex-validate-results--positions (make-hash-table :test #'equal))
          (list (list :error (vector "" "" (propertize "ERROR" 'face 'error) (cdr checked)))))
      (setq shex-validate-results--positions (cdr checked))
      (mapcar
       (lambda (quad)
         (pcase-let ((`(,node ,shape ,status ,reason) quad))
           (list (cons node shape)
                 (vector node shape
                         (propertize status 'face (shex-validate-results--status-face status))
                         ;; rudof's own `reason' strings routinely carry an
                         ;; embedded/trailing newline (e.g. "Shape passed
                         ;; ...\n") -- left alone, `tabulated-list-print'
                         ;; renders that as a literal line break, silently
                         ;; splitting one logical row across two buffer
                         ;; lines and breaking line-based row navigation.
                         (string-trim (replace-regexp-in-string "\n+" " " reason))))))
       (car checked)))))

(defun shex-validate-results-goto-node ()
  "Jump to the row at point's node in its data buffer, at the position
recorded as of this results buffer's last (re)validation -- see
`shex-validate-results--positions'. If the node occurs more than
once as a subject there, jumps to the first (topmost) occurrence."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (node (car id))
         (buffer shex-validate-results--data-buffer))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "Data buffer for this results buffer is no longer live"))
    (let ((spans (gethash node shex-validate-results--positions)))
      (unless spans
        (user-error "Node %s does not occur as a subject in %s" node buffer))
      (pop-to-buffer buffer)
      (goto-char (caar (last spans))))))

;;; Following point: highlighting the node/shape at point

(defface shex-validate-highlight
  '((t :inherit highlight))
  "Face for the node/shape of the result-shapemap row at point,
highlighted in the data/schema buffers -- see
`shex-validate-results--highlight-update'."
  :group 'shex-validate)

(defvar-local shex-validate-results--highlight-overlays nil
  "Overlays `shex-validate-results--highlight-update' put in the
data/schema buffers for the row currently at point.")

(defvar-local shex-validate-results--highlight-id nil
  "The `tabulated-list-get-id' last highlighted -- lets
`shex-validate-results--highlight-update' no-op while point
stays on the same row, e.g. on every character typed elsewhere.")

(defun shex-validate-results--clear-highlight ()
  (mapc #'delete-overlay shex-validate-results--highlight-overlays)
  (setq shex-validate-results--highlight-overlays nil))

(defun shex-validate--find-shape-decl (schema-buffer shape)
  "Return the `shape_expr_label' node in SCHEMA-BUFFER declaring SHAPE
\(a fully resolved IRI/`_:label' string, matching a
`rudof-validate-shex' quadruple's own SHAPE element), or nil.
Resolves each candidate label the same BASE/PREFIX-aware way
`shexc-ts-mode''s own flymake backend does
\(`shexc-shexj-resolve-label'), not by raw source text -- see
`shexc-ts-mode--flymake-undefined-shapes'."
  (with-current-buffer schema-buffer
    (let ((ctx (shexc-shexj-buffer-directive-ctx)))
      (seq-find
       (lambda (n) (equal (shexc-shexj-resolve-label ctx n) shape))
       (treesit-query-capture
        (treesit-buffer-root-node 'shexc)
        '((shape_expr_decl label: (shape_expr_label) @label))
        nil nil t)))))

(defun shex-validate-results--highlight-update ()
  "Highlight the row at point's node (in its data buffer) and shape
\(in its schema buffer), following point -- a buffer-local
`post-command-hook' in every `shex-validate-results-mode'
buffer. A node/shape with no resolvable location (see
`shex-validate--diagnostics'/`shex-validate--find-shape-decl')
is silently left unhighlighted rather than signaling an error on every
command."
  (let ((id (ignore-errors (tabulated-list-get-id))))
    (unless (equal id shex-validate-results--highlight-id)
      (shex-validate-results--clear-highlight)
      (setq shex-validate-results--highlight-id id)
      (when-let* ((node (car id))
                  (shape (cdr id))
                  (data-buffer shex-validate-results--data-buffer)
                  ((buffer-live-p data-buffer)))
        (dolist (span (gethash node shex-validate-results--positions))
          (let ((ov (make-overlay (car span) (cdr span) data-buffer)))
            (overlay-put ov 'face 'shex-validate-highlight)
            (push ov shex-validate-results--highlight-overlays)))
        (when-let* ((schema-buffer (buffer-local-value 'shex-validate-schema-buffer data-buffer))
                    ((buffer-live-p schema-buffer))
                    (label (shex-validate--find-shape-decl schema-buffer shape)))
          (let ((ov (make-overlay (treesit-node-start label) (treesit-node-end label) schema-buffer)))
            (overlay-put ov 'face 'shex-validate-highlight)
            (push ov shex-validate-results--highlight-overlays)))))))

(defvar-keymap shex-validate-results-mode-map
  :parent tabulated-list-mode-map
  "RET" #'shex-validate-results-goto-node
  "<mouse-2>" #'shex-validate-results-goto-node)

(define-derived-mode shex-validate-results-mode tabulated-list-mode "ShEx-Results"
  "Major mode for a ShEx validation result ShapeMap: one row per
node/shape association, `RET' jumps to that node in the data buffer.
Moving point over a row also highlights its node/shape in the
data/schema buffers -- see
`shex-validate-results--highlight-update'."
  (setq tabulated-list-format
        [("Node" 40 t) ("Shape" 30 t) ("Status" 14 t) ("Reason" 0 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'shex-validate-results--highlight-update nil t)
  (add-hook 'kill-buffer-hook #'shex-validate-results--clear-highlight nil t))

;;;###autoload
(defun shex-validate-show-result-shapemap (&optional data-buffer)
  "Show a `*ShEx Result ShapeMap: NAME*' buffer for DATA-BUFFER
\(default: the current buffer) in a bottom side window."
  (interactive)
  (let* ((data-buffer (or data-buffer (current-buffer)))
         (results-buffer (get-buffer-create (format "*ShEx Result ShapeMap: %s*" (buffer-name data-buffer)))))
    (with-current-buffer results-buffer
      (shex-validate-results-mode)
      (setq shex-validate-results--data-buffer data-buffer)
      (setq tabulated-list-entries
            (lambda () (shex-validate-results--entries data-buffer)))
      (tabulated-list-print))
    (display-buffer
     results-buffer
     '((display-buffer-in-side-window) (side . bottom) (window-height . 0.25)))))

(provide 'shex-validate)

;;; shex-validate.el ends here
