(in-package :stumpwm)

(defvar *groups-key-bindings* '("q" "w" "e" "a" "s" "d"))

(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
    `(run-commands ,@ns)))

(defun map-groups-keybindings ()
  (dotimes (i (length *groups-key-bindings*))
    (let ((group (nth i *groups-key-bindings*)))
      (define-key *root-map*
          (kbd (format nil "M-~A" group))
        (format nil "gselect ~A" (string-upcase (string-upcase group)))))))


(run-commands "grename Q")

(make-groups-bg "W" "E" "A" "S" "D")

(map-groups-keybindings)
