(in-package :stumpwm)

(export '(loadrc-forget
          now-playing
          sh-time
          sdcv
          set-backlight))

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

(defcommand sdcv () ()
  )

(defcommand set-backlight (level) ((:number "Set brightness level:"))
  (with-open-file (sys-backlight-file "/sys/class/backlight/intel_backlight/brightness"
                                      :direction :output :if-exists :overwrite)
    (format sys-backlight-file "~a~%" (* 100 level))))
