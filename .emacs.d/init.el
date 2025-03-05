;;; init.el ---
;; Ensure `package` is initialized
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Install missing packages
(dolist (pkg '(exwm evil which-key ivy naga-theme doom-modeline nix-modeline
                    lsp-mode lsp-ui company smartparens vterm company-box pipewire
		    undo-tree vterm-toggle rust-mode beacon buffer-move minibar dashboard ivy-posframe))
  (unless (package-installed-p pkg)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install pkg)))

(load-file '"~/.emacs.d/elpa/olivia/menu.el")
(load-file '"~/.emacs.d/elpa/olivia/efetch.el")
(load-file '"~/.emacs.d/elpa/olivia/olterm.el")
;; Load core packages
(require 'exwm)
(require 'evil)
(require 'which-key)
(require 'ivy)
(require 'naga-theme)
(require 'doom-modeline)
(require 'vterm)
(require 'company)
(require 'lsp)
(require 'lsp-ui)
(require 'smartparens-config)
(require 'pipewire)
(require 'undo-tree)
(require 'window)
(require 'beacon)
(require 'buffer-move)
(require 'exwm-randr)
(require 'project)



;; Enable company-mode globally
(global-company-mode 1)
(beacon-mode 1)

;; Configure company settings
(setq company-idle-delay 0.2)  ;; Time before suggestions appear
(setq company-minimum-prefix-length 2)  ;; Trigger completions after 2 characters
(setq company-tooltip-align-annotations t)  ;; Align annotations to the right
(setq company-tooltip-flip-when-above t)   ;; Flip tooltip if above cursor

(setq ring-bell-function 'ignore)
(setq visible-bell t)
;; Enable company-box for enhanced completion UI
(add-hook 'company-mode-hook 'company-box-mode)
;; Configure company backends (using `company-capf` for LSP integration)
(setq company-backends '((company-capf company-files)))

;; Keybindings for company
(define-key evil-normal-state-map (kbd "C-c C-c") 'company-complete)

;; LSP configuration for C/C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq lsp-clients-clangd-executable "clangd")
(setq lsp-prefer-capf t)  ;; Use completion-at-point for better integration



;; Enable LSP UI features
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq ivy-posframe-display-functions-alist
      '((t . ivy-posframe-display-at-frame-bottom-center)))

(add-hook 'c-mode-hook 'lsp-ui-mode)
(add-hook 'c++-mode-hook 'lsp-ui-mode)

;; Smartparens configuration for automatic bracket pairing(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'smartparens-mode)
(smartparens-global-mode)
(show-paren-mode t)

;; Doom modeline setup
(ivy-posframe-mode 1)
(doom-modeline-mode 1)
(load-theme 'naga 1)  ;; Load custom theme
(menu-bar-mode -1)    ;; Disable the menu bar
(tool-bar-mode -1)    ;; Disable the tool bar
(scroll-bar-mode -1)  ;; Disable the scroll bar
(global-set-key (kbd "C-c C-e") 'eval-buffer)
;; Enable Evil Mode for vim-like keybindings
(evil-mode 1)

;; Enable Which-key for keybinding hints
(which-key-mode 1)

;; Enable Ivy for better completions
(ivy-mode 1)
(minibar-mode 1)

;; EXWM setup (workspace, simulation keys, etc.)
(setq exwm-workspace-number 2)

(setq exwm-input-simulation-keys
      '(([?\C-b] . left)
        ([?\C-f] . right)
        ([?\C-p] . up)
        ([?\C-n] . down)
        ([?\C-a] . home)
        ([?\C-e] . end)
        ([?\M-v] . prior)
        ([?\C-v] . next)
        ([?\C-d] . delete)
        ([?\C-k] . (S-end delete))
        ([?\M-w] . C-c)
        ([?\C-w] . C-x)
        ([?\C-y] . C-v)))



;; EXWM RandR setup for multi-monitor configuration
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-0" 1 "DP-1"))

(start-process-shell-command
 "xrandr" nil
 "xrandr --output DP-1  --mode 1920x1080 --pos 0x0 --rotate normal \
          --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal")
(exwm-randr-mode 1)

(exwm-systemtray-mode 1)

(exwm-input-set-key (kbd "M-x") 'olivia/M-x)

(defun my/close-exwm-buffer-and-kill ()
  "Close the current EXWM buffer (window) and kill any normal buffers."
  (interactive)
  
  ;; Kill the current EXWM window by switching to another workspace or closing the current one
  (exwm-workspace-delete)
  
  ;; Kill the current buffer (Emacs buffer) if it's not the scratch buffer
  (unless (string= (buffer-name) "*scratch*")
    (kill-buffer)))

;; Unbind the LSP keybindings from s-l
(define-key lsp-mode-map (kbd "s-l") nil)

;; Bind LSP functions to s-S
(define-key lsp-mode-map (kbd "s-i") 'lsp-describe-session)  ;; or whichever LSP command you want


;; buffer manager
(exwm-input-set-key (kbd "s-<return>") 'eshell)
(exwm-input-set-key (kbd "s-M-<return>") 'olivia/new-vterm)
(exwm-input-set-key (kbd "s-p") 'kill-emacs)
(exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)
(exwm-input-set-key (kbd "<XF86AudioPlay>") 'olivia/evilMenu)
(exwm-input-set-key (kbd "s-=") (lambda () (interactive)(pipewire-increase-volume)))
(exwm-input-set-key (kbd "s--") (lambda () (interactive)(pipewire-decrease-volume)))
(exwm-input-set-key (kbd "s-0") (lambda () (interactive)(pipewire-toggle-muted)))
(exwm-input-set-key (kbd "s-M-h") (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-M-l") (lambda () (interactive) (exwm-workspace-switch 0)))
;; Evil mode keybindings for window navigation

(exwm-input-set-key (kbd "s-h") 'windmove-left)
(exwm-input-set-key (kbd "s-l") 'windmove-right)
(exwm-input-set-key (kbd "s-j") 'windmove-down)
(exwm-input-set-key (kbd "s-k") 'windmove-up)

(exwm-input-set-key (kbd "s-C-k") 'buf-move-up)
(exwm-input-set-key (kbd "s-C-j") 'buf-move-down)
(exwm-input-set-key (kbd "s-C-h") 'buf-move-left)
(exwm-input-set-key (kbd "s-C-l") 'buf-move-right)
(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "C-c l s") '(lambda () (interactive) (async-shell-command "slock" nil "slock")))


(defun olivia/detect-makefile-projects ()
  "Detect all directories in home containing a Makefile and add them as projects."
  (interactive)
  (let ((home-dir (expand-file-name "~"))  ;; Home directory
        (projects '()))  ;; List to store detected projects
    (dolist (dir (directory-files home-dir t))
      (when (and (file-directory-p dir)
                 (not (member (file-name-nondirectory dir) '("." "..")))
                 (file-exists-p (expand-file-name "Makefile" dir)))  ;; Check for Makefile
        (push dir projects)))  ;; Add directory to list if it contains Makefile
    (setq project--projects projects)  ;; Set projects for project.el
    (message "Detected projects: %s" projects)))  ;; Optionally display the list
;; Automatically detect Makefile projects when Emacs starts
(add-hook 'emacs-startup-hook 'olivia/detect-makefile-projects)


;; Call the function to detect projects when switching projects
(setq project-switch-commands '((project-find-file "Find file")
                                (project-find-regexp "Find regexp")
                                (project-find-dir "Find directory")
                                (project-vc "VCS status")))

;; Keybinding to switch between detected projects
(global-set-key (kbd "C-c p") 'project-switch-project)




(defun olivia/exwm-update-class()
  (exwm-workspace-rename-buffer exwm-title))

(add-hook 'exwm-update-title-hook
	  #'olivia/exwm-update-class) 

(defun insert-boilerplate-code ()
  "Insert boilerplate code for new files based on their type."
  (when (not (file-exists-p (buffer-file-name)))
    (cond
     ;; For C header files
     ((string-suffix-p ".h" (buffer-file-name))
      (insert "#pragma once\n\n"))


     ((string-suffix-p ".h" (buffer-file-name))
      (insert "#include <iostream>\n\nint main() {return 0;}"))
     
     ((string-suffix-p ".c" (buffer-file-name))
      (insert "#include <stdio.h>\n\nint main() {return 0;}"))

     ;; For Python files
     ((string-suffix-p ".py" (buffer-file-name))
      (insert "import  os"))
     
     ((string-suffix-p ".sh" (buffer-file-name))
      (insert "#!/usr/bin/env bash\n"))
     ;; For Emacs Lisp files
     ((string-suffix-p ".el" (buffer-file-name))
      (insert (format ";; %s.el\n\n" (file-name-base (buffer-file-name))))))))

(add-hook 'find-file-hook 'insert-boilerplate-code)

(global-display-line-numbers-mode 1)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))


(defun my/evil-quit-advice (&rest _)
  "Prevent `evil-quit` from closing the last frame in EXWM."
  (if (and exwm-enabled
           (= (length (frame-list)) 1) ;; Only one frame
           (= (length (buffer-list)) 1)) ;; Only one buffer
      (message "Cannot close the last buffer while in EXWM!")
    t)) ;; Otherwise, allow quitting

(advice-add 'evil-quit :before-while #'my/evil-quit-advice)
;; Start EXWM
(exwm-enable)

;; Optional: Start an initial buffer (scratch buffer)
(setq initial-buffer-choice t)

;; Keybinding for LSP formatting
(define-key evil-normal-state-map (kbd "C-c f") 'lsp-format-buffer)

;; Keybinding for LSP code actions
(define-key evil-normal-state-map (kbd "C-c a") 'lsp-execute-code-action)

;; Keybinding for code completion (Company mode)
(define-key evil-normal-state-map (kbd "C-c C-c") 'company-complete)

;; Start picom for transparency and effects
(start-process "picom" nil "picom" "-b")

(setq-default line-spacing 2)

;;(setq-default line-spacing 2)
;;Additional setup for better performance (disable GC too early)
(setq gc-cons-threshold (* 50 1000 1000)) ;; 50MB before GC is triggered

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(naga))
 '(custom-safe-themes
   '("96cc35ec4a0b6ac2aa45549ddbafd488b0fce9d38f60d29a6c7b7f9e5cafb0ed"
     default))
 '(package-selected-packages
   '(adwaita-dark-theme async-status basic-theme beacon binclock
			buffer-move camera company-box
			company-posframe dashboard
			doom-modeline-now-playing dynamic-graphs
			dynamic-spaces efire ellama emoji-display
			emoji-github enotify erc-colorize
			erc-scrolltoplace erc-twitch erc-youtube
			eterm-256color exwm-firefox-evil exwm-surf
			exwm-x fireplace flycheck frog-menu gc-buffers
			gdscript-mode glsl-mode google-maps
			gradle-mode helm-twitch htmlize ivy-emoji
			ivy-posframe ivy-youtube jabber keycast llm
			lsp-ivy lsp-java lsp-javacomp lsp-ui
			mini-header-line mini-modeline minibar
			minibuffer-header naga-theme nasm-mode
			nix-modeline org-notify pipewire popwin
			recently rust-mode simple-modeline smartparens
			sml-modeline steam svg-clock telephone-line
			treemacs-evil treesit-auto undo-tree
			vim-tab-bar vlc vterm-toggle
			which-key-posframe zen-mode zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
