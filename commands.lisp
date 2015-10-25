(in-package :stumpwm)

(defcommand loadrc-forget () () "Reload the @file{~/.stumpwmrc} file without remember current state."
  (handler-case
   (progn
     (with-restarts-menu (load-rc-file nil)))
   (error (c)
          (message "^B^1*Error loading rc file:^n ~A" c))
   (:no-error (&rest args)
              (declare (ignore args))
              (message "rc file loaded successfully."))))

(defcommand now-playing () ()
  (message "~A" (run-shell-command "mpc --format \"[[%artist% - ]%title%]\"| head -1" T)))

(defcommand sh-time () ()
  (message "~A" (run-shell-command "date \"+%H:%M [%e %b, %A]\"" T)))
