;; logos-mode.el --- major mode for logos  -*- lexical-binding: t; -*-

(defvar *logos-host* "localhost")
(defvar *logos-port* nil)
(defvar *logos-connection* nil)
(defvar *logos-proof-buffer* "*Proof*")
(defvar *eval-marker* 1)

(defun new-logos-session ()
  (interactive)
  (let ((current-window (selected-window))
	(new-window     (split-window-right)))
    (select-window new-window)
    (switch-to-buffer *logos-proof-buffer*)
    (read-only-mode)
    (select-window current-window)
    (logos-create-connection)))

(defmacro with-proof-buffer (&rest body)
  (declare (indent defun))
  (let ((current-window (gensym))
	(proof-window   (gensym)))
    `(progn
       (let ((,current-window (selected-window))
	     (,proof-window   (get-buffer-window
			       *logos-proof-buffer*)))
	 (select-window ,proof-window)
	 ;; Check if this is proof window
	 (when buffer-read-only
	   (setq buffer-read-only nil))
	 (progn
	   ,@body)
	 (read-only-mode)
	 (select-window ,current-window)))))

(defun get-next-step! ()
  (goto-char *eval-marker*)
  (re-search-forward (regexp-quote "."))
  (let* ((new-eval-marker (point))
	 (command (buffer-substring
		   *eval-marker* (- new-eval-marker 1))))
    (setq *eval-marker* new-eval-marker)
    command))

(defun logos-run-next-command (command)
  (let* ((url (logos-create-connection))
	 (postfix "/one-step")
	 (args    (format "\\\?command=%s"
			  (url-hexify-string command))))
    (shell-command-to-string
     (format "curl -s -X POST %s%s%s" url postfix args))))

(defun logos-one-step ()
  (interactive)
  (let* ((command (get-next-step!))
	 (result  (logos-run-next-command command)))
    (with-proof-buffer
     (kill-region (point-min) (point-max))
     (insert result))))

(defun logos-create-connection ()
  (interactive)
  (if *logos-connection*
      *logos-connection*
    (let ((port nil))
      (if *logos-port*
	  (setq port *logos-port*)
	(let ((port-number (read-number "Port: ")))
	  (setq *logos-port* port-number)
	  (setq port port-number)))
      (setq *logos-connection* (format "%s:%s" *logos-host* port))
      *logos-connection*)))

(defun logos-quit ()
  (setq *logos-connection* nil
	*logos-port* nil))

;;;###autoload
(define-derived-mode logos-mode prog-mode "logos"
  "Major mode for nps files."
  ;;:abbrev-table nps-mode-abbrev-table
  ;;(setq font-lock-defaults nps--font-lock-defaults)
  ;;(setq-local comment-start "#")
  ;;(setq-local comment-start-skip "#+[\t ]*")
  ;;(setq-local indent-line-function #'nps-indent-line)
  ;;(setq-local indent-tabs-mode t)
  )
