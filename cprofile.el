

(defun cprofile-init ()
  "Setup the Python environment."
  (pymacs-exec "import cProfile_emacs"))

(defun cprofile-load-stats (stat-file)
  "Load a cProfile output file, return the pstats.Stats objects."
  (pymacs-call "cProfile_emacs.EmacsStats" stat-file))

; todo: replace lexical-let with emacs24 lexical bindings
(defun cprofile-create-stats-buffer (stat-file)
  "Create a buffer for displaying stats for a cprofile output file."
  (with-current-buffer (get-buffer-create (concat "*Stats " stat-file "*"))
    (lexical-let ((stats (cprofile-load-stats stat-file))
                  (buf (current-buffer)))
      (progn
        (setq map (make-sparse-keymap))
        (define-key map "c" 
          #'(lambda () (interactive) (cprofile-display-cumulative buf stats)))
        (define-key map "t" 
          #'(lambda () (interactive) (cprofile-display-time buf stats)))
        (use-local-map map)
        (hl-line-mode 1)
        (cprofile-display-stats buf stats)))))
  
(defun cprofile-display-stats (buffer stats)
  (with-current-buffer buffer
    (let ((nline (- (window-body-height) 12)))
      (progn
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (pymacs-call "cProfile_emacs.EmacsStats.display" stats nline))
        (goto-char (point-min))
        (beginning-of-line 9)
        (setq buffer-read-only 1)))))

(defun cprofile-display-cumulative (buffer stats)
  (cprofile-sort-cumulative stats)
  (cprofile-display-stats buffer stats))

(defun cprofile-display-time (buffer stats)
  (cprofile-sort-time stats)
  (cprofile-display-stats buffer stats))

(defun cprofile-sort-cumulative (stats)
  (pymacs-call "cProfile_emacs.EmacsStats.sort" stats "cum"))

(defun cprofile-sort-time (stats)
  (pymacs-call "cProfile_emacs.EmacsStats.sort" stats "time"))

(cprofile-init)
(cprofile-create-stats-buffer "/home/ghamon/log")


