
(require 'ivy)
(require 'exwm)



(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))



(defun olivia/pacman ()
  "Use ivy to interactively run pacman/yay commands."
  (interactive)
  (let* ((commands '(("Install" . "sudo yay -S") 
                     ("Update (Pacman)" . "sudo pacman -Syu") 
                     ("Update (Yay)" . "yay -Syu") 
                     ("Reinstall" . "sudo pacman -S --reinstall") 
                     ("Remove" . "sudo pacman -Rns") 
                     ("Search (Pacman)" . "pacman -Ss") 
                     ("Search (Yay)" . "yay -Ss") 
                     ("Clean Cache" . "sudo pacman -Sc")
                     ("Clean Orphans" . "sudo pacman -Rns $(pacman -Qdtq)")))
         (choice (ivy-read "Select Pacman/Yay Command: " 
                           (mapcar #'car commands)))
         (command (cdr (assoc choice commands)))
         (pkg (if (string-match-p "Search\\|Install\\|Reinstall\\|Remove" choice)
                  (read-string "Package Name: "))))
    (when (and (string= pkg "") (string-match-p "pkg" command))
      (error "This command requires a package name."))
    (let ((full-command (if pkg (concat command " " pkg) command)))
      (async-shell-command full-command "*Pacman Output*"))))


(defun olivia/evilMenu ()
  "A menu to evaluate Elisp in different contexts: file, cursor, buffer, or expressions."
  (interactive)
  (ivy-read "EL load: "
            '("file" "cursor" "buffer" "expression")
            :action (lambda (selection)
                      (cond
                       ;; Evaluate Elisp from a file
                       ((string= selection "file")
                        (let ((file (read-file-name "Load Elisp file: " default-directory nil t)))
                          (when (and file (file-exists-p file))
                            (load-file file)
                            (message "Loaded file: %s" file))))

                       ;; Evaluate the expression around the cursor
                       ((string= selection "cursor")
                        (let ((expr (thing-at-point 'sexp t)))
                          (if expr
                              (condition-case err
                                  (message "Result: %S" (eval (read expr)))
                                (error (message "Error: %s" err)))
                            (message "No valid expression at point."))))

                       ;; Evaluate an expression for the current buffer
                       ((string= selection "buffer")
                        (ivy-read "Buffer Expression> "
                                  nil
                                  :action (lambda (expr)
                                            (message "%S" (eval (read expr))))))

                       ;; Evaluate arbitrary Elisp expression interactively
                       ((string= selection "expression")
                        (let ((input (read-string "Elisp> ")))
                          (condition-case err
                              (message "Result: %S" (eval (read input)))
                            (error (message "Error: %s" err)))))))))

(defvar olivia/search-web-history nil "History for web searches.")
(defvar olivia/youtube-search-history nil "History for YouTube searches.")
(defvar olivia/url-https-history nil "History for HTTPS URLs.")
(defvar olivia/url-http-history nil "History for HTTP URLs.")

(defun olivia/save-histories ()
  "Save history variables to a file."
  (interactive)
  (with-temp-file "~/.emacs.d/olivia-histories.el"
    (insert (format "(setq olivia/search-web-history '%S)\n" olivia/search-web-history))
    (insert (format "(setq olivia/youtube-search-history '%S)\n" olivia/youtube-search-history))
    (insert (format "(setq olivia/url-https-history '%S)\n" olivia/url-https-history))
    (insert (format "(setq olivia/url-http-history '%S)\n" olivia/url-http-history))
    ))

;; Automatically save history when Emacs is killed
(add-hook 'kill-emacs-hook 'olivia/save-histories)


(defun olivia/load-histories ()
  "Load history variables from a file."
  (load "~/.emacs.d/olivia-histories.el" t))

;; Load history at startup
(add-hook 'emacs-startup-hook 'olivia/load-histories)

(defun olivia/M-x ()
  (interactive)
  (ivy-read (concat (getenv "USER") ": ")
            '(
	      "M-x"
              "🕸web-search"
              "web-search-youtube"
              "web-url-https"
              "web-url-http"
              "run-shell"
              "eval"
              "erun"
              "emoji"
              "close-buffer"
              "pacman"
              "lsp"
              "term-past"
	      )
            :action (lambda (selection)
		      (cond
                       ((string= selection "🕸web-search")
                        (ivy-read "Enter search query: "
                                  (delete-dups olivia/search-web-history)
                                  :action (lambda (query)
                                            (add-to-list 'olivia/search-web-history query)
                                            (browse-url (concat "https://duckduckgo.com/?t=ffab&q=" (url-hexify-string query))))))

                       ((string= selection "web-search-youtube")
                        (ivy-read "Enter YouTube search query: "
                                  (delete-dups olivia/youtube-search-history)
                                  :action (lambda (query)
                                            (add-to-list 'olivia/youtube-search-history query)
                                            (browse-url (concat "https://www.youtube.com/results?search_query=" (url-hexify-string query))))))

                       ((string= selection "web-url-https")
                        (ivy-read "Enter HTTPS URL: "
                                  (delete-dups olivia/url-https-history)
                                  :action (lambda (query)
                                            (add-to-list 'olivia/url-https-history query)
                                            (browse-url (concat "https://" query)))))

                       ((string= selection "web-url-http")
                        (ivy-read "Enter HTTP URL: "
                                  (delete-dups olivia/url-http-history)
                                  :action (lambda (query)
                                            (add-to-list 'olivia/url-http-history query)
                                            (browse-url (concat "http://" query)))))

                       ((string= selection "run-shell")
                        (ivy-read "Shell: "
                                  (split-string (shell-command-to-string "compgen -c") "\n")
                                  :action (lambda (cmd)
                                            (start-process cmd nil cmd))))

                       ((string= selection "erun")
                        (eshell))

                       ((string= selection "emoji")
                        (emoji-search))

                       ((string= selection "term-past")
                        (term-paste))

                       ((string= selection "eval")
                        (olivia/evilMenu))

                       ((string= selection "close-buffer")
                        (kill-buffer))

                       ((string= selection "lsp")
                        (lsp))

                       ((string= selection "pacman")
                        (olivia/pacman))

                       ((string= selection "M-x")
                        (call-interactively 'execute-extended-command))))))


(provide 'olivia-menu)
