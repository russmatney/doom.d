;;;  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after markdown-mode
   (:map markdown-mode-map
     "<backspace>" nil
     "<A-left>"    nil
     "<A-right>"   nil
     "A-<tab>"     #'markdown-cycle)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

(setq org-refile-targets '(("~/Dropbox/todo/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/todo/someday.org" :level . 1)
                           ("~/Dropbox/todo/writing.org" :maxlevel . 2)
                           ("~/Dropbox/todo/routines.org" :level . 1)
                           ("~/Dropbox/todo/tickler.org" :maxlevel . 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org
(map!
 (:after org
   (:map org-mode-map
     "C-j"    #'evil-window-down
     "C-k"    #'evil-window-up
     "M-h"    nil
     "M-v"    #'evil-paste-after
     "A-RET"  #'org-insert-item
     "A-t"    #'org-set-tags
     "TAB"    #'+org/toggle-fold
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org capture
(map!
 (:after org-capture
   (:map org-capture-mode-map
     [remap evil-save-and-close]          #'org-capture-finalize
     [remap evil-save-modified-and-close] #'org-capture-finalize
     [remap evil-quit]                    #'org-capture-kill)))

(defun +russ/org-capture-hook ()
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/Dropbox/todo/tickler.org" "Tickler")
                                 "* %i%? \n %U"))

        org-agenda-files '("~/Dropbox/todo/inbox.org"
                           "~/Dropbox/todo/gtd.org"
                           "~/Dropbox/todo/tickler.org")))

(after! org-capture
  (+russ/org-capture-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! org-clubhouse)

(map!
 (:leader
   (:desc "notes" :prefix "n"
     :desc "add story to clubhouse" :n "c" #'org-clubhouse-create-story)))

