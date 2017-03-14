;; Visual stuff
;;(menu-bar-mode -1) ; hide menu bar
(setq inhibit-startup-message t)
(tool-bar-mode -1) ; hide tool bar
(scroll-bar-mode -1) ; hide the scroll bar
(setq show-paren-delay 0) ; no delay in highlighting
(show-paren-mode 1) ; highlight matching parentheses
(setq-default indent-tabs-mode nil) ; tabs are evil
(setq-default tab-width 2) ; 2 spaces per tab
(setq column-number-mode t) ; show columns in mode line as well
(setq ns-pop-up-frames nil) ; stops creation of extra frame
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(set-frame-font "Monaco-13" nil t)
(set-frame-parameter nil 'fullscreen 'maximized)

(setq enable-remote-dir-locals t) ; search for .dir-locals.el when using tramp

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; load emacs package managers
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (package-initialize)
  (add-to-list 'package-archives source t))

;; Required packages
;; Will automatically check if those packages are
;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar jmt/packages
  ;; list packages here with spaces inbetween
  '(auctex
    auto-complete
    auto-complete-c-headers
    auto-complete-auctex
    elpy
    flycheck
    helm
    helm-swoop
    highlight-current-line
    soft-charcoal-theme
    ))
(dolist (p jmt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;verilog
(setq verilog-auto-newline nil)

;;flycheck
(add-hook 'after-save-hook #'global-flycheck-mode)

;;spell checking
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "--list")

;;highlight current line
(require 'highlight-current-line)
;(highlight-current-line-on t)
;(set-face-background 'highlight-current-line-face "#000000")

;;start helm
(require 'helm-config)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-swoop)

;; start auto-complete with emacs
(require 'auto-complete)
;; do defult config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start t)
(setq ac-auto-show-menu 1)
(global-auto-complete-mode t)

(setq-default c-basic-offset 2)
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

;; python editiing
(elpy-enable)
(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; latex edititing
(require 'auto-complete-auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq latex-run-command "pdflatex")
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

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
(global-set-key (kbd "C-c C-o") 'kill-whitespace)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
;(global-whitespace-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (flatland)))
 '(custom-safe-themes
   (quote
    ("a164837cd2821475e1099911f356ed0d7bd730f13fa36907895f96a719e5ac3e" "7bfd38733dc58478d2104c30114022a88ddb92540fa4fb7516f79e55967a348d" "78f614a58e085bd7b33809e98b6f1a5cdd38dae6257e48176ce21424ee89d058" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "9ab634dcc9131f79016c96c4955298409649f6538908c743a8a9d2c6bc8321ef" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" default)))
 '(fci-rule-color "#202325")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#1f2124")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0000")
     (40 . "#ff4a52")
     (60 . "#f6aa11")
     (80 . "#f1e94b")
     (100 . "#f5f080")
     (120 . "#f6f080")
     (140 . "#41a83e")
     (160 . "#40b83e")
     (180 . "#b6d877")
     (200 . "#b7d877")
     (220 . "#b8d977")
     (240 . "#b9d977")
     (260 . "#93e0e3")
     (280 . "#72aaca")
     (300 . "#8996a8")
     (320 . "#afc4db")
     (340 . "#cfe2f2")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141719" :foreground "#f8f8f8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Monaco")))))
(put 'erase-buffer 'disabled nil)

;;; .emacs ends here
