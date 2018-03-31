;; -*- no-byte-compile: t; -*-
;;; private/russ/packages.el

(package! flycheck-mix)
(package! flycheck-credo)
(package! paredit)
(package! intero)
(package! ghc)
(package! dash)

(package! org-clubhouse
  :recipe (:fetcher github
           :repo "urbint/org-clubhouse"
           :files ("*")))

(package! zen-mode
  :recipe (:fetcher github
           :repo "aki237/zen-mode"
           :files ("*")))

(package! flow-minor-mode)
(package! company-flow)
(package! flycheck-flow)
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)

(package! lsp-mode)
(package! lsp-ui :recipe (:fetcher github :repo "emacs-lsp/lsp-ui"))
(package! lsp-haskell)
(package! lsp-rust)
(package! company-lsp)

(package! lsp-css
  :recipe (:fetcher github
               :repo "emacs-lsp/lsp-css"))

(package! graphql-mode)

(package! helm)
