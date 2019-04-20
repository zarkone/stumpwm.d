(ql:quickload :dbus)

(in-package :stumpwm)

(defun open-dbus-session-connection ()
  (dbus:open-connection
          (make-instance 'iolib.multiplex:event-base)
          (dbus:session-server-addresses)))

(defun get-current-layout ()
  (let ((upower_conn (open-dbus-session-connection)))

    (dbus:authenticate (dbus:supported-authentication-mechanisms upower_conn) upower_conn)

    (dbus:hello upower_conn)

    (dbus:invoke-method upower_conn "getCurrentLayout"
                        :path "/ru/gentoo/KbddService"
                        :destination "ru.gentoo.KbddService"
                        :interface "ru.gentoo.kbdd")))
