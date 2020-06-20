(ql:quickload :swank)
;; (ql:quickload :clx-truetype)

(in-package :stumpwm)

(defvar *confdir* "/home/zarkone/.stumpwm.d")
(defun load-conf-file (filename)
  (load (format nil "~A/~A" *confdir* filename)))

(load-conf-file "keys.lisp")
;; (load-conf-file "modem.lisp")
(load-conf-file "modeline.lisp")
(load-conf-file "windows.lisp")
(load-conf-file "commands.lisp")
(load-conf-file "groups.lisp")
(load-conf-file "bar.lisp")




;; (set-prefix-key (kbd "F20"))
(set-prefix-key (kbd "C-q"))
;; (set-prefix-key (kbd "Menu"))

;; (load-module "ttf-fonts")
;; (xft:cache-fonts)

;; (set-font (make-instance 'xft:font :family "Droid Sans Mono" :subfamily "Regular" :size 10))

;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 12))

;; (set-font (make-instance 'xft:font :family "Liberation Mono" :subfamily "Regular" :size 10))
;; (in-package :clx-truetype)
;; (loop for key being the hash-keys of (gethash "Ubuntu" *font-cache*)
        ;; do (print key))
;; (set-font "-*-terminus-medium-r-normal-*-16-320-*-*-*-*-iso10646-1")

(setf *mouse-focus-policy* :sloppy)

;; (pushnew "/home/zarkone/.stumpwm-git/stumpwm-dbus/" asdf:*central-registry*)
;; (asdf:oos 'asdf:load-op :stumpwm.contrib.dbus)
;; (stumpwm.contrib.dbus:open-connection)

(swank-loader:init)
(defcommand swank () ()
  (setf stumpwm:*top-level-error-action* :break)
  ;; (when (not (getf (swank:connection-info) :pid))
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string
   (current-screen)
   "Starting swank. M-x slime-connect RET RET, then (in-package :stumpwm)."))

(swank)
