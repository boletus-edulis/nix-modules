(in-package :stumpwm)

(setq *startup-message* "^5Lisp ^2^bsystem operational.")

(setq *message-window-gravity* :center)
(setq *input-window-gravity* :center)

(defparameter *terminal* "alacritty")
(defparameter *browser* "firefox")

(defcommand firefox () ()
  (run-shell-command "firefox"))

(defcommand alacritty () ()
  (run-shell-command "alacritty"))

(define-key *root-map* (kbd "c") *terminal*)
(define-key *root-map* (kbd "C-c") *terminal*)

(setf *mode-line-position* :top)
(setf *mouse-focus-policy* :click) ;; :click, :ignore, :sloppy
