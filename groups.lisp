(in-package :stumpwm)

(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
    `(run-commands ,@ns)))

(defun map-groups-keybindings ()
  (dotimes (n (length *groups-key-bindings*))
    (define-key *root-map*
        (kbd (format nil "M-~A" (nth n *groups-key-bindings*)))
      (format nil "gselect ~A" (+ 1 n)))))

(defvar *groups-key-bindings* '(1 2 3 "q" "w" "e"))

(run-commands "grename code")
(make-groups-bg "www" "term" "chat")
(map-groups-keybindings)
