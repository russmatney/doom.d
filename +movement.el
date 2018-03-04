;;; +movement.el --- movement helper functions -*- lexical-binding: t; -*-

(defun rm/move-window-right ()
   ""
   (interactive)
   (if (window-in-direction 'right)
     (evil-window-right 1)
     (shell-command-to-string "chunkc tiling::window --focus east")
   )
)

(defun rm/move-window-left ()
   ""
   (interactive)
   (if (window-in-direction 'left)
     (evil-window-left 1)
     (shell-command-to-string "chunkc tiling::window --focus west")
   )
)

(defun rm/move-window-above ()
   ""
   (interactive)
   (if (window-in-direction 'above)
     (evil-window-up 1)
     (shell-command-to-string "chunkc tiling::window --focus north")
   )
)

(defun rm/move-window-below ()
   ""
   (interactive)
   (if (window-in-direction 'below)
     (evil-window-down 1)
     (shell-command-to-string "chunkc tiling::window --focus south")
   )
)


(defun wpcarro/tmux-emacs-windmove (dir)
  "Move windows in a Tmux-friendly way."
  (let* ((dir->opts '((left . ("-L" . windmove-left))
                      (right . ("-R" . windmove-right))
                      (up . ("-U" . windmove-up))
                      (down . ("-D" . windmove-down))))
         (tmux-opt (->> dir->opts
                        (alist-get dir)
                        car))
         (emacs-fn (->> dir->opts
                        (alist-get dir)
                        cdr))
         (other-window (windmove-find-other-window dir))
         (at-edge? (null other-window))
         (other-is-inactive-minibuffer?
          (and (window-minibuffer-p other-window)
               (not (minibuffer-window-active-p other-window)))))
    (if (or at-edge? other-is-inactive-minibuffer?)
        (shell-command (format "tmux select-pane %s" tmux-opt))
      (funcall emacs-fn))))


(defun wpcarro/tmux-emacs-windmove-left ()
  (interactive)
  (wpcarro/tmux-emacs-windmove 'left))

(defun wpcarro/tmux-emacs-windmove-right ()
  (interactive)
  (wpcarro/tmux-emacs-windmove 'right))

(defun wpcarro/tmux-emacs-windmove-up ()
  (interactive)
  (wpcarro/tmux-emacs-windmove 'up))

(defun wpcarro/tmux-emacs-windmove-down ()
  (interactive)
  (wpcarro/tmux-emacs-windmove 'down))
