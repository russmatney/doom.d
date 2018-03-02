;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep 'evil)
  (load! +evil)
  (load! +git)
  (load! +leader)
  (load! +commands)
  (load! +helm)
  (load! +helm-mini)
  (load! +company)
  (load! +neotree)
  (load! +haskell)
  (load! +org)
  (load! +private)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From Old Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; --- Global keybindings ---------------------------
 :nvime "M-x" #'execute-extended-command
 :nvime "A-x" #'execute-extended-command

 "A-<backspace>"     #'backward-kill-word

 :n "A-e" #'next-error
 :n "A-E" #'previous-error

 ;; Copy and paste
 "M-c"    #'evil-yank
 "M-v"    #'clipboard-yank
 ;; hide
 "M-h"   #'ns-do-hide-emacs

 ;; quick hops
 :m "C-j" #'+russ:multi-next-line
 :m "C-k" #'+russ:multi-previous-line

 "M-p"   #'projectile-find-file
 :n  "A-v"  #'evil-window-vsplit
 :n  "A-s"  #'evil-window-split

 :v "="  #'evil-indent
)

(map!
 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur  ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter   ; preview
     ))

     ; "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "M-v" #'yank
   "M-z" #'undo
   "C-r" #'evil-paste-from-register
   "C-k" #'ivy-previous-line
   "C-j" #'ivy-next-line
   "C-l" #'ivy-alt-done
   "C-h" #'ivy-backward-kill-word
   "C-w" #'ivy-backward-kill-word
   "C-u" #'ivy-kill-line
   "C-b" #'backward-word
   "C-f" #'forward-word)

 ;; markdown
 (:after markdown-mode
   (:map markdown-mode-map
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil
     "A-<tab>"     #'markdown-cycle))

 ;; help mode
 (:map help-mode-map
   :n "[["  #'help-go-back
   :n "]]"  #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! +movement)

(map!
 ;; movement
 "M-w"    #'ace-window
 "A-h"    #'rm/move-window-left
 "A-j"    #'rm/move-window-below
 "A-k"    #'rm/move-window-above
 "A-l"    #'rm/move-window-right

 ;; splits
 "A-\\"    #'evil-window-vsplit
 "A--"    #'evil-window-split

 ;; popups
 "C-`"    #'+popup/toggle

 ;; size Adjustments
 "S-<left>"  #'evil-window-increase-width
 "S-<right>" #'evil-window-decrease-width

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; next, previous
 :n  "]b" #'next-buffer
 :n  "A-B" #'next-buffer
 :n  "[b" #'previous-buffer
 :n  "A-b" #'previous-buffer
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load projectile workspace func here?
;; (load! +)

(map!
 ;; Workspaces
 "A-c"    #'+workspace/new
 "A-,"    #'+workspace/rename
 "A-L"    #'+workspace/load
 "A-S"    #'+workspace/save
 "A-P"    #'rs/projectile-switch-project-workspace

 ;; switch to
 ;; "A-s"    #'+workspace/switch-to
 "A-n"    #'+workspace/switch-left
 "A-p"    #'+workspace/switch-right
 "A-["    #'+workspace/switch-left
 "A-]"    #'+workspace/switch-right

 ;; switch by number
 "A-1"    (λ! (+workspace/switch-to 1))
 "A-2"    (λ! (+workspace/switch-to 2))
 "A-3"    (λ! (+workspace/switch-to 3))
 "A-4"    (λ! (+workspace/switch-to 4))
 "A-5"    (λ! (+workspace/switch-to 5))
 "A-6"    (λ! (+workspace/switch-to 6))
 "A-7"    (λ! (+workspace/switch-to 7))
 "A-8"    (λ! (+workspace/switch-to 8))
 "A-9"    (λ! (+workspace/switch-to 9))
 "A-0"    (λ! (+workspace/switch-to 0))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; eval exp and buffer
 :n "A-;"    #'eval-last-sexp
 :n "A-:"    #'eval-buffer
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar +russ-dir (file-name-directory load-file-name))

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

(set-register ?d '(file . "~/dotfiles/"))
(set-register ?u '(file . "~/projects/urbint/"))

(setq
 whitespace-line-column 100
 whitespace-style
 '(face trailing lines-tail)
 )
(global-whitespace-mode t)

(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))

(setq +ivy-buffer-icons t)


(add-to-list 'auto-mode-alist
             '("rc\\'" . (lambda () (conf-mode)))
             '("\\.rkt\\'" . (lambda () (geiser))))

(def-package! racket-mode
  :mode "\\.rkt\\'"
  :config
    (geiser-mode)
  )


;; elm
(add-hook! elm-mode
  (flycheck-mode))

;; rust
(add-hook! rust-mode
  (flycheck-mode)
  (rainbow-delimiters-mode)
  )

;; elixir
(add-hook! elixir-mode
  ;; :mode "\\.exs?$"
  ;; :config
  (flycheck-mode)
  (turn-off-smartparens-mode)
  (rainbow-delimiters-mode)
  ;; (setq alchemist-goto-elixir-source-dir "/path/to/elixir/source/")
  ;; (setq alchemist-goto-erlang-source-dir "/path/to/erlang/source/")

  )

;; (defun custom-erlang-mode-hook ()
;;   (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

;; (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)



(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))


;; emacs-lisp
(add-hook! :append 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
(add-hook! :append 'emacs-lisp-mode-hook (flycheck-mode 0))

;; clojure
(def-package! clojure-mode
  :mode "\\.cljs?$"
  :config
  (company-mode)
  (flycheck-mode)
  (rainbow-delimiters-mode)
  (setq cider-repl-display-help-banner nil)
  (setq cider-prompt-for-symbol nil))

(def-package! cider
  :after clojure-mode)

(def-package! flycheck-clojure
  :after clojure-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-clojure-setup))

(def-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! org-clubhouse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! dired
  :init
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :config
  (map!
    :n "-" #'dired-jump
    :map dired-mode-map
         :n "-" #'dired-up-directory
         :n "<return>" #'dired-find-file
         ))


(add-hook! 'before-save-hook 'whitespace-cleanup)
