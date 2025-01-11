;; efetch.el


(defun efetch ()
  "Display the Emacs bootloader, system information, and Emacs info in a buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*efetch*"))
        (system-info (concat "System Information:\n"
                            (format "  OS: %s\n" system-name)
                            (format "  Machine: %s\n" system-name) ;; Correct usage of system-name
                            (format "  Host: %s\n" system-name)
                            (format "  User: %s\n" user-login-name)
                            (format "  Shell: %s\n" (getenv "SHELL"))
                            (format "  Home Directory: %s\n" (getenv "HOME"))
                            (format "  Current Time: %s\n" (current-time-string))
                            (format "  Load Average: %s\n" (string-join (mapcar 'number-to-string (load-average)) " "))))
        (emacs-info (concat "\nEmacs Information:\n"
                           (format "  Emacs Version: %s\n" emacs-version)
                           (format "  Emacs Build Info: %s\n" (if (boundp 'emacs-build-time) emacs-build-time "Unknown"))
                           (format "  Emacs Start Time: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
                           (format "  Emacs PID: %d\n" (emacs-pid))
                           (format "  Process ID: %d\n" (emacs-pid))
                           (format "  Byte-compiled: %s\n" (if byte-compile-current-file "Yes" "No"))
                           (format "  Init File: %s\n" (or user-init-file "Unknown"))
                           (format "  Executable: %s\n" (or invocation-name "Unknown"))))
        (environment-info (concat "\nEnvironment Variables:\n"
                                 (format "  SHELL: %s\n" (getenv "SHELL"))
                                 (format "  HOME: %s\n" (getenv "HOME"))
                                 (format "  PATH: %s\n" (getenv "PATH"))
                                 (format "  LANG: %s\n" (getenv "LANG"))
                                 (format "  DISPLAY: %s\n" (getenv "DISPLAY"))
                                 (format "  TERM: %s\n" (getenv "TERM"))
                                 (format "  Emacs Load Path: %s\n" load-path)))
        (resources-info (concat "\nSystem Resources:\n"
                               (format "  CPU: %s\n" (shell-command-to-string "cat /proc/cpuinfo | grep 'model name' | uniq"))
                               (format "  Memory: %s\n" (shell-command-to-string "free -h | grep Mem | awk '{print $3 \" / \" $2}'"))
                               (format "  Disk Usage: %s\n" (shell-command-to-string "df -h | grep '^/dev' | awk '{print $3 \" / \" $2}'"))))
        (packages-info (concat "\nLoaded Packages:\n"
                               (format "  Package Manager: %s\n" package-archive-priorities)
                               (format "  Loaded Packages: %s\n" (mapconcat 'symbol-name package-activated-list ", ")))))
    (with-current-buffer buffer
      (insert (concat
               (propertize "==============================\n" 'face '(:foreground "green" :weight bold))
               (propertize "     Welcome to Emacs!       \n" 'face '(:foreground "cyan" :weight bold))
               (propertize "==============================\n\n" 'face '(:foreground "green" :weight bold))

               ;; Emacs Bootloader section
               (propertize "Emacs Bootloader\n" 'face '(:foreground "yellow" :weight bold))
               "This is your Emacs instance.\n"
               "Loading configuration...\n\n"

               ;; System Information Section
               (propertize "System Information:\n" 'face '(:foreground "magenta" :weight bold))
               system-info

               ;; Emacs Information Section
               (propertize "\nEmacs Information:\n" 'face '(:foreground "blue" :weight bold))
               emacs-info

               ;; Environment Variables Section
               (propertize "\nEnvironment Variables:\n" 'face '(:foreground "cyan" :weight bold))
               environment-info

               ;; System Resources Section
               (propertize "\nSystem Resources:\n" 'face '(:foreground "green" :weight bold))
               resources-info

               ;; Loaded Packages Section
               (propertize "\nLoaded Packages:\n" 'face '(:foreground "purple" :weight bold))
               packages-info)))
    (goto-char (point-min))
    (switch-to-buffer buffer)
    (goto-char (point-min))))

(exwm-input-set-key (kbd "s-d s-f") 'efetch)
