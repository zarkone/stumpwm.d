(in-package :stumpwm)


(defvar *terminal* "gnome-terminal"
  "Command to start a terminal.")

(defcommand exec-term (cmd) (:string)
 (run-commands (format nil "exec ~A -x ~A" *terminal* cmd)))

(defcommand void-cmd () () nil)

(define-key *root-map* (kbd "RET")
  (format nil "exec ~A" *terminal*))

(define-key *root-map* (kbd "a") "sh-time")

(define-key *root-map* (kbd "y") "exec ~/big-monitor.sh")
(define-key *root-map* (kbd "Y") "exec xrandr --auto")

(define-key *root-map* (kbd "B") "mode-line")
(define-key *root-map* (kbd "b") "banish")
(define-key *root-map* (kbd "F12") "loadrc-forget")

(define-key *root-map* (kbd "=") "gother")
(define-key *root-map* (kbd "C-=") "gother")
(define-key *root-map* (kbd "C-q") "other-window")
(define-key *root-map* (kbd "[") "pull-hidden-other")

;; execute apps map
(undefine-key *root-map* (kbd "c"))

(defvar *exec-map* nil)
(setf *exec-map*
  (let ((c (make-sparse-keymap)))
    (define-key c (kbd "w") "exec ~/Apps/bin/chrome")
    (define-key c (kbd "W") "exec firefox")
    (define-key c (kbd "C") "exec gnome-control-center")
    (define-key c (kbd "e") "exec emacsclient -ca \"\"")
    (define-key c (kbd "g") "exec-term ncmpcpp")
    (define-key c (kbd "t") "exec-term \"tmux new-session -A -s scratch\"")
    (define-key c (kbd "z") "exec-term htop")
    (define-key c (kbd "a") "exec-term mc")
    (define-key c (kbd "A") "exec-term ranger")
    (define-key c (kbd "d") "exec nautilus")
    (define-key c (kbd "p") "exec pavucontrol")
    (define-key c (kbd "m") "exec emacsclient -c -e '(unread-mail)'")
    (define-key c (kbd "y") "exec-term wicd-curses")
    (define-key c (kbd "f") "exec nmcli con up wavespace_X_team")
    (define-key c (kbd "r") "exec redshift")
    (define-key c (kbd "l") "exec slock")
    (define-key c (kbd "R") "exec killall redshift")
    (define-key c (kbd "Pause") "exec mts-enable")
    c))

(define-key *root-map* (kbd "c") '*exec-map*)

(defvar *brightness-map* nil)
(setf *brightness-map*
      (let ((b (make-sparse-keymap)))
        (loop for level from 1 below 10 by 1 do
             (define-key b (kbd (write-to-string level))
               (format nil "set-backlight ~a" level)))
        b))

(define-key *root-map* (kbd "u") '*brightness-map*)
(define-key *top-map* (kbd "XF86MonBrightnessDown") "set-backlight 1")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "set-backlight 9")

;; some control commands

(define-key *top-map* (kbd "s-Down") "run-shell-command amixer -c0 set Master 3dB-")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "run-shell-command amixer -c0 set Master 3dB-")
(define-key *top-map* (kbd "s-Up") "run-shell-command amixer -c0 set Master 3dB+")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "run-shell-command amixer -c0 set Master 3dB+")

(define-key *top-map* (kbd "s-/") "run-shell-command mpc toggle")
;; (define-key *top-map* (kbd "XF86AudioPlay") "run-shell-command mpc toggle")
(define-key *top-map* (kbd "s-.") "run-shell-command mpc next")
;; (define-key *top-map* (kbd "XF86AudioNext") "run-shell-command mpc next")
(define-key *top-map* (kbd "s-,") "run-shell-command mpc prev")

(define-key *root-map* (kbd "SunPrint_Screen") "run-shell-command scrot")
(define-key *root-map* (kbd "C-SunPrint_Screen") "run-shell-command scrot -s")

(define-key *root-map* (kbd "d") "exec-term sdcv")
(define-key *root-map* (kbd "D") "exec-term sdcv-sel")

(define-key *root-map* (kbd "M") "now-playing")
(define-key *top-map* (kbd "ISO_Next_Group") "void-cmd")
