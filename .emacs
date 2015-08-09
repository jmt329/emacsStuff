;; visual stuff
(menu-bar-mode -1) ; hide menu bar
(tool-bar-mode -1) ; hide tool bar
(setq show-paren-delay 0) ; no delay in highlighting
(show-paren-mode 1) ; highlight matching parentheses
(setq-default indent-tabs-mode nil) ; tabs are evil
(setq column-number-mode t) ; show columns in mode line as well
(electric-pair-mode 1) ; instert paired braces
;; make GUI better (but still not great)
(add-to-list 'default-frame-alist '(height . 44)) ; make it start taller
(setq ns-pop-up-frames nil) ; stops creation of extra frame

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
  '(ample-theme auctex auto-complete auto-complete-c-headers elpy flymake-cppcheck
                flymake-cursor yasnippet))
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

;; verilog editting
(setq verilog-auto-newline nil)

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
 '(ansi-color-names-vector
   ["#504545" "#ad8572" "#a9df90" "#aaca86" "#91a0b3" "#ab85a3" "#ddbc91" "#bdbdb3"])
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
   (quote
    ("e8a9dfa28c7c3ae126152210e3ccc3707eedae55bdc4b6d3e1bb3a85dfb4e670" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "de8fa309eed1effea412533ca5d68ed33770bdf570dcaa458ec21eab219821fd" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
