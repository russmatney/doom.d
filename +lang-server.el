;;;  -*- lexical-binding: t; -*-

;;; Pasted from https://github.com/hlissner/doom-emacs/issues/460#issue-304477158
;;; lang/language-server/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :after (:any cc-mode
               c-mode
               c++-mode
               objc-mode
               python
               haskell-mode
               rust-mode
               caml-mode
               vue-mode
               css-mode
               scss-mode
               sass-mode
               less-mode
               stylus-mode)
  :config
  (setq lsp-enable-eldoc t)
  (setq lsp-enable-completion-at-point t))

(def-package! lsp-ui
  :after lsp-mode
  :config
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-flycheck-enable t)
    (map! :map lsp-ui-mode-map
        [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
        [remap xref-find-references] #'lsp-ui-peek-find-references))
  :hook
  (lsp-mode . lsp-ui-mode))

(def-package! company-lsp
  :after (:all lsp-mode lsp-ui)
  :config
  (set! :company-backend lsp-mode 'company-lsp)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t))
