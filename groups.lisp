(in-package :stumpwm)

(defvar *groups-key-bindings* '(1 2 3 4 5 6 7 8 9))

(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
    `(run-commands ,@ns)))

(defun map-groups-keybindings ()
  (dotimes (n (length *groups-key-bindings*))
    (define-key *root-map*
        (kbd (format nil "M-~A" (nth n *groups-key-bindings*)))
      (format nil "gselect ~A" (+ 1 n)))))



(run-commands "grename code")
(make-groups-bg "www" "term" "chat")
(map-groups-keybindings)
