;;------------------------------------------------------------------------------
;; Visual stuff
;;------------------------------------------------------------------------------
(menu-bar-mode -1) ; hide menu bar
(setq inhibit-startup-message t)
(tool-bar-mode -1) ; hide tool bar
(scroll-bar-mode -1) ; hide the scroll bar
(setq show-paren-delay 0) ; no delay in highlighting
(show-paren-mode 1) ; highlight matching parentheses
(setq-default indent-tabs-mode nil)
(setq column-number-mode t) ; show columns in mode line as well
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(setq line-number-display-limit-width 2000000) ; stop ?? line numbers
(setq-default truncate-lines t)
(setq scroll-step            1
      scroll-conservatively  10000) ; scroll 1 line at a time
(display-time-mode 1) ; show time in modeline

;;------------------------------------------------------------------------------
;; load emacs package managers
;;------------------------------------------------------------------------------
(require 'package)
(require 'compile)
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
  '(
    helm
    helm-swoop
    ))
(dolist (p jmt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;------------------------------------------------------------------------------
;; eshell
;;------------------------------------------------------------------------------
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;------------------------------------------------------------------------------
;;verilog
;;------------------------------------------------------------------------------
(defun jmt-verilog-config ()
  "For use in `verilog-mode-hook'."
  (setq verilog-auto-newline nil)
  (local-set-key (kbd "C-c C-q") 'search-verilog-boundaries))
;; add to hook
(add-hook 'verilog-mode-hook 'jmt-verilog-config)

;-------------------------------------------------------------------------------
;;highlight current line
;-------------------------------------------------------------------------------
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#000000")

;;------------------------------------------------------------------------------
;;helm
;;------------------------------------------------------------------------------
(require 'helm-config)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;(global-set-key (kbd "C-c C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-swoop-without-pre-input)
(global-set-key (kbd "C-C M-i") 'helm-multi-swoop-all)
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.3)))

;;------------------------------------------------------------------------------
;;My functions
;;------------------------------------------------------------------------------
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

;; Batten Extend-o
(defun extend-char-to-end ()
 "Extend adjacent character to fill 80 columns"
  (interactive)
  (let ( (end-point (point-at-eol)) (match-char nil) )
    (beginning-of-line)
    (re-search-forward "[^ ][ ]*$" end-point t)
    (setq match-char (string-to-char (match-string 0)))
    (replace-match "")
    (insert-char match-char (- 80 (current-column)))))
(global-set-key (kbd "C-q") 'extend-char-to-end)

(defun search-verilog-boundaries ()
  "Run occur for verilog boundary words"
  (interactive)
  (occur "^\s*\\(\\(function\\)\\|\\(class\\)\\|\\(task\\)\\|\\(endfunction\\)\\|\\(endclass\\)\\|\\(endtask\\)\\|\\(virtual\\)\\)"))

(defun jt-occur ()
  "Run occur for `JT:'"
  (interactive)
  (occur "JT:"))
(global-set-key (kbd "C-c C-j") 'jt-occur)

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun align-repeat (start end regexp)
    "Repeat alignment with respect to 
     the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end 
        (concat "\\(\\s-*\\)" regexp) 1 1 t))

;;------------------------------------------------------------------------------
;; qgrep
;;------------------------------------------------------------------------------
(let ((default-directory "~/.emacs.d/qgrep/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(autoload 'qgrep "qgrep" "Quick grep" t)
(autoload 'qgrep-no-confirm "qgrep" "Quick grep" t)
(autoload 'qgrep-confirm "qgrep" "Quick grep" t)
(global-set-key (kbd "\C-c g") 'qgrep-no-confirm)
(global-set-key (kbd "\C-c G") 'qgrep-confirm)
;; Stricter filters
(setq wrs-qgrep-default-find-format "find %s \\( -wholename '*/.svn' -o -wholename '*/obj' -o -wholename '*/.git' -o -wholename '*/sim' -o -wholename '*/VCOMP' \\) -prune -o -type f \\( '!' -name '*atdesignerSave.ses' -a \\( '!' -name '*~' \\) -a \\( '!' -name '#*#' \\) -a \\( '!' -name '*.soma' \\) -a \\( '!' -name '*.vp' \\) -a \\( -name '*' \\) \\) -type f -print0")
(setq qgrep-default-find (format wrs-qgrep-default-find-format "."))
(setq qgrep-default-grep "grep -iI -nH -e \"%s\"")

;;------------------------------------------------------------------------------
;;Misc
;;------------------------------------------------------------------------------
;; comfirm emacs exit
(setq confirm-kill-emacs 'yes-or-no-p)

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c f") 'find-file-at-point)

;; F5 to revert buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;;(setq jmt-snippet-directory "~/.emacs.d/snippets")
;;(yasnippet-enable)
;; override default snippets
;;(setq yas/root-directory (append '(jmt-snippet-directory)
;;                                 '(yas/root-directory)
 ;;                                ))
;;(yas/load-directory jmt-snippet-directory)



;; Make auto-revert-tail-mode work again
(when (or (> emacs-major-version 24) ; Emacs 25.0.90 needs this as well
          (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
  (setq auto-revert-use-notify nil))

;; make ediff better
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(setq ediff-control-frame-upward-shift 40)
(setq ediff-narrow-control-frame-leftward-shift -30)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; let me open large files
(setq large-file-warning-threshold 1000000000) ; 1GB

;; regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#eee8d5")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
