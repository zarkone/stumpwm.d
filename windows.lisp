(in-package :stumpwm)

(setf *window-border-style* :tight)
(setf *maxsize-border-width* 2)
(setf *normal-border-width* 2)
(setf **transient-border-width** 2)
(set-focus-color "#464646")

;; like kbdd, for per-window language layout.
;; (pushnew "/home/zarkone/.stumpwm-git/clx-xkeyboard/" asdf:*central-registry*)
;; (load "/home/zarkone/.stumpwm-git/clx-xkeyboard/test/stumperwindowlayout.lisp")
;; (run-commands "enable-per-window-layout")
;; (run-commands "disable-per-window-layout")

(defun reorder-windows-handler (win)
  "if you want window order to be from 1, not from 0"
  (let ((sorted-windows (sort-windows (current-group))))
    (do ((num 0 (+ num 1))
         (windows sorted-windows (cdr windows)))

        ((null (car windows)))
      (setf (window-number (car windows)) num))))

(add-hook *new-window-hook* 'reorder-windows-handler)
(add-hook *destroy-window-hook* 'reorder-windows-handler)
;; (remove-hook *new-window-hook* 'reorder-windows-handler)
;; (remove-hook *destroy-window-hook* 'reorder-windows-handler)
(defvar *windows-key-bindings* '(1 2 3 4 5 6 7 8 9))

(defun map-windows-keybindings ()
  (dotimes (n (length *windows-key-bindings*))
    (define-key *root-map*
        (kbd (format nil "~A" (nth n *windows-key-bindings*)))
      (format nil "select-window-by-number ~A" (+ 1 n)))
    (define-key *root-map*
        (kbd (format nil "C-~A" (nth n *windows-key-bindings*)))
      (format nil "pull ~A" (+ 1 n)))))


(map-windows-keybindings)
