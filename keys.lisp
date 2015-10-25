(in-package :stumpwm)


(defvar *terminal* "urxvtcd"
  "Command to start a terminal.")

(defcommand exec-term (cmd) (:string)
    (run-commands (format nil "exec ~A -e ~A" *terminal* cmd)))

(defcommand void-cmd () () nil)

(define-key *root-map* (kbd "RET")
  (format nil "exec ~A" *terminal*))

(define-key *root-map* (kbd "a") "sh-time")

(define-key *root-map* (kbd "B") "mode-line")
(define-key *root-map* (kbd "F12") "loadrc-forget")

(define-key *root-map* (kbd "=") "gother")
(define-key *root-map* (kbd "[") "pull-hidden-other")

;; execute apps map
(undefine-key *root-map* (kbd "c"))

(defvar *exec-map* nil)
(setf *exec-map*
  (let ((c (make-sparse-keymap)))
    (define-key c (kbd "w") "exec google-chrome")
    (define-key c (kbd "W") "exec firefox")
    (define-key c (kbd "e") "exec emacsclient -ca \"\"")
    (define-key c (kbd "g") "exec-term ncmpcpp")
    (define-key c (kbd "z") "exec-term htop")
    (define-key c (kbd "a") "exec-term mc")
    (define-key c (kbd "p") "exec pavucontrol")
    (define-key c (kbd "d") "exec dolphin")
    (define-key c (kbd "m") "exec emacsclient -c -e '(unread-mail)'")
    (define-key c (kbd "y") "exec-term wicd-curses")
    (define-key c (kbd "r") "exec redshift")
    (define-key c (kbd "R") "exec killall redshift")
    (define-key c (kbd "Pause") "exec mts-enable")
    c))

(define-key *root-map* (kbd "c") '*exec-map*)
;; some control commands

(define-key *top-map* (kbd "s-Down") "run-shell-command amixer -c0 set Master 3dB-")
(define-key *top-map* (kbd "s-Up") "run-shell-command amixer -c0 set Master 3dB+")

(define-key *top-map* (kbd "s-/") "run-shell-command ncmpcpp toggle")
(define-key *top-map* (kbd "s-.") "run-shell-command ncmpcpp next")
(define-key *top-map* (kbd "s-,") "run-shell-command ncmpcpp prev")

(define-key *root-map* (kbd "SunPrint_Screen") "run-shell-command scrot-pics")
(define-key *root-map* (kbd "C-SunPrint_Screen") "run-shell-command scrot-pics -s")

(define-key *root-map* (kbd "d") "exec-term sdcv")
(define-key *root-map* (kbd "D") "exec-term sdcv-sel")

(define-key *root-map* (kbd "M") "now-playing")
(define-key *top-map* (kbd "ISO_Next_Group") "void-cmd")
