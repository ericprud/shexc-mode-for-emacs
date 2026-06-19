;; Project-specific PREFIX map for shexc-ts-mode, demonstrating
;; `shexc-ts-mode-prefix-map' as a list -- see extends-person.shex in
;; this same directory.
;;
;; `ex:' here is NOT in the bundled "rdfa" map (it's a made-up
;; per-project namespace, like most `ex:'/`ns:'-style prefixes in
;; real schemas), so without this file `M-x shexc-ts-mode-insert-prefix'
;; would fail with "Prefix `ex:' is not in any of the active prefix
;; maps", and flymake would just flag it as "Undefined prefix ex:"
;; with no suggested fix.  With this file's "my-project" map listed
;; *before* "rdfa", both now resolve `ex:' to this project's own
;; namespace -- and anything `my-project' doesn't define (`rdf:',
;; `xsd:', ...) still falls through to "rdfa" unchanged.
;;
;; The first time you visit a file here, Emacs will ask whether to
;; trust this directory's `.dir-locals.el' (since its `eval' entry runs
;; code) -- answer "!" to trust it permanently, or add the form below
;; to `safe-local-eval-forms' yourself if you'd rather not be asked
;; again for similar files elsewhere.
((nil . ((eval . (when (derived-mode-p 'shexc-ts-mode)
                    (setq-local shexc-ts-mode-prefix-maps
                                (cons '("my-project"
                                        :description "Prefixes specific to this example project."
                                        :prefixes (("ex" . "http://a.example/ns#")))
                                      shexc-ts-mode-prefix-maps))
                    (setq-local shexc-ts-mode-prefix-map '("my-project" "rdfa")))))))
