;; Add to exec-path
(push "/usr/local/bin" exec-path)

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
        rhtml-mode))
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
                      starter-kit-bindings
                      solarized-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load theme
(load-theme 'solarized-dark t)
