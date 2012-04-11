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

; Display the column number in the status bar.
(setq column-number-mode t)

;; Change Emacs font size
(set-face-attribute 'default nil :height 100)

;; Start Emacs server
(server-start)

;; Make the minibuffer display unfinished commands instantly
(setq echo-keystrokes 0.001)

;; Dedicated windows
;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  "Toggles whether the selected window is dedicated."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(global-set-key "\C-cd" 'toggle-current-window-dedication)

;; Display date and time in status bar
(setq display-time-day-and-date t)
(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable Evil mode in all buffers
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

;; Org-mode
(defun always-insert-item ()
     (interactive)
     (if (not (org-in-item-p))
       (insert "\n- ")
       (org-insert-item)))

(add-hook 'org-mode-hook
          (lambda ()
(evil-define-key 'normal org-mode-map "O" (lambda ()
                                            (interactive)
                                            (end-of-line)
                                            (org-insert-heading t)
                                            (evil-append nil)
                                            ))

(evil-define-key 'normal org-mode-map "o" (lambda ()
                                            (interactive)
                                            (end-of-line)
                                            (always-insert-item)
                                            (evil-append nil)
                                            ))

(evil-define-key 'normal org-mode-map "t" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (evil-append nil)
                     ))

(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-define-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style

(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)
))

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
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
             '("labpc-[0-9][0-9].cs.princeton.edu" nil "/ssh:dskang@portal:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't allow shell prompt to be deleted
(setq comint-prompt-read-only t)

;; Don't echo line in comint-mode
(defun echo-false-comint ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'echo-false-comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab width
(setq-default tab-width 4)

;; C indentation
(setq-default c-basic-offset 4)

;; Javascript indentation
(setq js-indent-level 4)

;; Auto-indentation
(dolist (hook '(c-mode-hook
                java-mode-hook
                html-mode-hook
                rhtml-mode-hook
                css-mode-hook
                php-mode-hook
                js-mode-hook))
  (add-hook hook '(lambda () (local-set-key "\C-m" 'reindent-then-newline-and-indent))))

(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on auto pairs globally
(electric-pair-mode)

;; Don't automatically compile after saving in SCSS mode
(setq scss-compile-at-save nil)

;; Enable whitespace mode everywhere
(global-whitespace-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/Dropbox/notes/"
                         "~/Dropbox/courses/cos333/"
                         "~/Dropbox/courses/cos448/"
                         "~/Dropbox/courses/cos461/"
                         "~/Dropbox/courses/egr495/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq erc-fill-column 72)
;; Ignore useless messages
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "338" "353" "477"))

;; Save ERC logs
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels-directory "~/.erc/")
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)
