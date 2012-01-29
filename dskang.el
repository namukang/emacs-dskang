;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use command key as meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)

;; Scroll one line at a time
(setq scroll-step 1)

;; Don't ask for confirmation upon creating new buffer or file
(setq confirm-nonexistent-file-or-buffer 'nil)

;; Turn off visible bell
(setq visible-bell nil)

;; Ignore prompt when killing a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Change Emacs font size
(set-face-attribute 'default nil :height 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)
(add-to-list 'load-path (concat esk-user-dir "/evil"))
(require 'evil)
(evil-mode 1)

;; Use 'jk' as ESC
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/684
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))
;; Show red box if in Emacs mode
(setq evil-emacs-state-cursor '("red" box))

;; Move using visual lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; General commands
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",q" 'kill-buffer) ; quit
(define-key evil-normal-state-map ",f" 'ido-find-file) ; find file
(define-key evil-normal-state-map ",b" 'ido-switch-buffer) ; show buffers
(define-key evil-normal-state-map ",o" 'org-agenda)  ; show org agenda

;; Lisp
(evil-define-key 'visual emacs-lisp-mode-map ",," 'eval-region)

;; Add "j" and "k" keybindings to some Emacs modes
(add-hook 'org-mode-hook
          (lambda ()
(define-key org-agenda-mode-map "j" 'evil-next-line)
(define-key org-agenda-mode-map "k" 'evil-previous-line)
))

(add-hook 'package-menu-mode-hook
          (lambda ()
(define-key package-menu-mode-map "j" 'evil-next-line)
(define-key package-menu-mode-map "k" 'evil-previous-line)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq comint-prompt-read-only t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-indentation
(dolist (hook '(c-mode-hook
                java-mode-hook
                html-mode-hook
                css-mode-hook
                php-mode-hook
                js-mode-hook))
  (add-hook hook '(lambda () (local-set-key "\C-m" 'reindent-then-newline-and-indent))))

(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))
