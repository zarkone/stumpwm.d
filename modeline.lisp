(in-package :stumpwm)

(load-module "battery-portable")
(load-module "wifi")
(load-module "maildir")
;; "[^B%n^b] %W"
;; "%n%s%t"
;; "%m%n%s%50t"
;; "%a %b %e %k:%M:%S"

(setf *bar-med-color* "^B^8")
(setf *bar-hi-color* "^B^3")
(setf *bar-crit-color* "^B^1")

(setf *colors*
      '("black"
       "red"
       "green"
       "yellow"
       "blue"
       "magenta"
       "cyan"
       "white"
       "GreenYellow"
       "#009696"))
(update-color-map (current-screen))

(setf *group-format* " %t ")
;; (setf *window-format* "%m%50t ")
(setf *window-format* "%m%n%s%20t ")
(setf *mode-line-timeout* 2)

(setf *time-modeline-string* "^9 %e %b^n^B %l:%M ^b")

(defun get-date-modeline ()
  (stumpwm:run-shell-command
   (format nil "date +\"~A\""
           *time-modeline-string*) t))

(defun get-layout-modeline ()
  (if (= 0 (get-current-layout *display*))
      "^3 en ^n"
      "^3^R ru ^r^n"))


(defvar jabber-message-count "0")
(setf jabber-message-count "0")

(defun get-jabber-message-count ()
  (if (equal "0" jabber-message-count)
    " • "
    (format nil "^R ~A ^r" jabber-message-count)))

(get-jabber-message-count)
(setf *screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^> "
            '(:eval (get-layout-modeline))
            "  "
            "^3%M^n"
            '(:eval (get-jabber-message-count))
            "^B^2^n^b%B "
            ;; '(:eval (get-date-modeline))
            ))

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000809")
(setf *mode-line-foreground-color* "DeepSkyBlue")

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
