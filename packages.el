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
