;; visual stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-default-font "Monaco-14")
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
;; make GUI better
(add-to-list 'default-frame-alist '(height . 44))
(setq ns-pop-up-frames nil)

;; load emacs package managers
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ;;("marmalade" . "http://marmalade-repo.org/packages/")
                  ;;("elpa" . "http://tromey.com/elpa/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Required packages
;; Will automatically check if those packages are
;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar jmt/packages
  ;; list packages here with spaces inbetween
  '(auto-complete auto-complete-c-headers color-theme-solarized elpy flymake-cppcheck flymake-cursor yasnippet))
(dolist (p jmt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; start auto-complete with emacs
(require 'auto-complete)
;; do defult config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; function which initializes auto-complete-c-headers and gets called for
;; c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/Library/Developer/CommandLineTools/usr/bin/../lib/clang/6.1.0/include")
  )

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; turn on Semantic
(semantic-mode 1)
;; adds semantic as a suggestion backend to auto complete and hooks this
;; function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; To get project includes in another directory working
;; turn on ede mode
;; (global-ede-mode 1)
;; ;; create a project for our program.
;; (ede-cpp-root-project "my project" :file "~/demos/my_program/src/main.cpp"
;;                       :include-path '("/../my_inc"))
;; ;; you can use system-include-path for setting up the system header file locations.
;; ;; turn on automatic reparsing of open buffers in semantic
;; (global-semantic-idle-scheduler-mode 1)

;;; cppcheck for c/c++ files
(add-hook 'c++-mode-hook 'flymake-cppcheck-load)
(add-hook 'c-mode-hook 'flymake-cppcheck-load)

;; python editiing
(elpy-enable)

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#232323" :foreground "#A5C25B" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Monaco"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "#666666")))))
