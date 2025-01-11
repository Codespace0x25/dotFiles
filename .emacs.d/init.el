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
		    undo-tree vterm-toggle rust-mode beacon buffer-move minibar dashboard))
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
(require 'exwm-randr)
(require 'beacon)
(require 'buffer-move)



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

(defun my/exwm-randr-setup ()
  "Configure screen layout with EXWM-Randr."
  (start-process-shell-command
   "xrandr" nil
   "xrandr --output DP-1  --mode 1920x1080 --pos 0x0 --rotate normal \
            --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal")
  (exwm-randr-enable))

(my/exwm-randr-setup)
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
(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreed)

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
(start-process "virmant" nil "virt-manager")

(setq-default line-spacing 2)

;;(setq-default line-spacing 2)
;;Additional setup for better performance (disable GC too early)
(setq gc-cons-threshold (* 50 1000 1000)) ;; 50MB before GC is triggered

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5809de220efea8a12353469ae27afeb4c1138c6cb50b19703b37c9cc1c8767df" "448d7e6f9639189b0196dd43047f3d8e018a28a9d3318e64eea35699d93a535d" "a68ec832444ed19b83703c829e60222c9cfad7186b7aea5fd794b79be54146e6" default))
 '(package-selected-packages
   '(emoji-display enotify zen-mode eterm-256color lsp-ivy erc-youtube ivy-youtube emoji-github pip projectile dashboard exwm-surf exwm-firefox-evil rust-mode ivy-emoji gameoflife frog-menu camera helm-twitch recently erc-colorize erc-scrolltoplace twitch twitch-api erc-twitch steam minibar helm-posframe which-key-posframe posframe company-posframe ivy-posframe minibuffer-header doom-modeline-now-playing awesome-tray telephone-line buffer-move becken beacon dashboard-ls dynamic-spaces dynamic-graphs gc-buffers async llm ellama request vterm-toggle binclock undo-tree adwaita-dark-theme fireplace pipewire company-box vterm nix-modeline nix-mode doom-modeline naga-theme nage ivy which-key exwm evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
