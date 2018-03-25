;;; .doom.d/+helm-mini.el -*- lexical-binding: t; -*-

(after! helm
    (defclass my-helm-source-file-buffers-class (helm-source-buffers)
      ((candidates :initform
                   (lambda ()
                     (mapcar 'buffer-name
                             (cl-remove-if-not #'buffer-file-name (buffer-list)))))))

    (defclass my-helm-source-other-buffers (helm-source-buffers)
      ((candidates :initform
                   (lambda ()
                     (mapcar 'buffer-name
                             (cl-remove-if #'buffer-file-name (buffer-list)))))))

    (setq
        my-helm-source-file-buffers-list (helm-make-source "File-Buffers" 'my-helm-source-file-buffers-class)
        my-helm-source-other-buffers-list (helm-make-source "Other" 'my-helm-source-other-buffers))

    (defvar helm-source-emacs-commands-history
      (helm-build-sync-source "Emacs commands history"
        :candidates (lambda ()
                      (let ((cmds))
                        (dolist (elem extended-command-history)
                          (push (intern elem) cmds))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "Emacs commands history")

    (defvar helm-source-my-org-files
      (helm-build-sync-source "Org Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/Dropbox/todo/inbox.org"
          "~/Dropbox/todo/tickler.org"
          "~/Dropbox/todo/collector.org"
          "~/Dropbox/todo/routines.org"
          "~/Dropbox/todo/gtd.org"
          "~/Dropbox/todo/someday.org"
          )))

    (defvar rm/quick-config-files
      (helm-build-sync-source "Config Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/.doom.d/config.el"
          "~/.doom.d/packages.el"
          "~/.doom.d/init.el"
          )))

  (setq helm-mini-default-sources '(my-helm-source-file-buffers-list
                                    my-helm-source-other-buffers-list
                                    helm-source-recentf
                                    helm-source-projectile-projects
                                    helm-source-my-org-files
                                    helm-source-emacs-commands-history
                                    helm-source-buffer-not-found
                                    rm/quick-config-files
                                    )))
