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

(defcommand lock-screen () ()
  (run-shell-command "(xfce4-screensaver-command --lock & ); sleep 1; xset dpms force off"))
(define-key *root-map* (kbd "C-l") "lock-screen")

(defcommand nodpms () ()
  (run-shell-command "xset s off -dpms"))

(define-key *root-map* (kbd "c") *terminal*)
(define-key *root-map* (kbd "C-c") *terminal*)

(setf *mode-line-position* :top)
(setf *mouse-focus-policy* :click) ;; :click, :ignore, :sloppy

;; gtk3 mousewheel fix
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

;; high dpi disable
(setf (getenv "QT_AUTO_SCREEN_SCALE_FACTOR") "1")
(setf (getenv "QT_SCALE_FACTOR") "1.7")
(setf (getenv "QT_FONT_DPI") "96")
(setf (getenv "GDK_SCALE") "2")
(setf (getenv "GDK_DPI_SCALE") "1")

;; (setf (getenv "SEGFAULT_SIGNALS") "bus abrt segv")
;; (setf (getenv "LD_PRELOAD") "/home/apfel/.guix-profile/lib/libSegFault.so")
;; (setf (getenv "MALLOC_CHECK_") "3")
;; (setf (getenv "MALLOC_MMAP_THRESHOLD_") "0")
;; (setf (getenv "MALLOC_PERTURB_") "1")
;; (setf (getenv "MALLOC_TRIM_THRESHOLD_") "1")

;; playground
;;(defcommand mytest () ()
;;  (window-send-string (concat "asdf" (string #\newline))))
