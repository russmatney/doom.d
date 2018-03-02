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

