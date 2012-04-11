;; Add to exec-path
(push "/usr/local/bin" exec-path)

;; These variables change the way Evil is loaded
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)

;; Install el-get if it is not present
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))
(setq el-get-sources
      '(el-get
        evil
        markdown-mode
        rhtml-mode
        php-mode-improved
        yaml-mode))
(el-get 'sync el-get-sources)

;; Add Marmalade as a package source
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Install my packages if they are not present
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(starter-kit
                      starter-kit-bindings)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
