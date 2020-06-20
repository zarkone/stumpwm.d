(in-package :stumpwm)
(ql:quickload :dbus)


#|
(dbus::get-property bus
                        "org.freedesktop.ModemManager1"
                        "/org/freedesktop/ModemManager1/Modem/6"
                        "org.freedesktop.ModemManager1.Modem"
                        "SignalQuality"
                     )
(dbus::with-introspected-object (modem bus "/org/freedesktop/ModemManager1/Modem/6" "org.freedesktop.ModemManager1")
      (modem "org.freedesktop.ModemManager1" "Modem"))
|#

(defun get-dbus-modem-property (property)
  (dbus::with-open-bus (bus (dbus::system-server-addresses))
    (dbus::get-property bus
                        "org.freedesktop.ModemManager1"
                        "/org/freedesktop/ModemManager1/Modem/6"
                        "org.freedesktop.ModemManager1.Modem"
                        property)))

(defun get-modem-signal-percent ()
  (car
   (get-dbus-modem-property "SignalQuality")))


(defun get-modem-phone-number ()
  (car
   (get-dbus-modem-property "OwnNumbers")))

(defun get-modem-state ()
  (car
   (get-dbus-modem-property "State")))
