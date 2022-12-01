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
(setq compilation-scroll-output t) ; scroll automatically
(setq ring-bell-function 'ignore) ; disable alarm bell

;; -----------------------------------------------------------------------------
;; mac
;; -----------------------------------------------------------------------------
(defun my-comint-init ()
  (setq comint-process-echoes t))

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (add-hook 'comint-mode-hook 'my-comint-init))

;;------------------------------------------------------------------------------
;; backup files
;;------------------------------------------------------------------------------
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;;------------------------------------------------------------------------------
;; packages
;;------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)

(require 'compile)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package ws-butler
  :ensure t
  :load-path "~/.emacs.d/ws-butler"
  :hook ((prog-mode . ws-butler-mode)))

(use-package helm
  :ensure t
  :bind (("C-x C-b" . 'helm-buffers-list)
         ("M-y"     . 'helm-show-kill-ring)
         ("M-x"     . 'helm-M-x))
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.3))))

(use-package doom-themes
  :load-path "~/.emacs.d/emacs-doom-themes"
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-Iosvkem t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;;(doom-themes-org-config))
  )

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package systemrdl-mode
  :load-path "~/.emacs.d/systemrdl-mode"
  :config
  (setq auto-mode-alist (cons '("\\.rdl.*" . systemrdl-mode) auto-mode-alist))
)

;;------------------------------------------------------------------------------
;; eshell
;;------------------------------------------------------------------------------
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;------------------------------------------------------------------------------
;; verilog
;;------------------------------------------------------------------------------
(defun jmt-verilog-config ()
  "For use in `verilog-mode-hook'."
  (setq verilog-auto-newline nil)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "C-c C-q") 'search-verilog-boundaries))
;; add to hook
(add-hook 'verilog-mode-hook 'jmt-verilog-config)

;;------------------------------------------------------------------------------
;; My functions
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

(defun simrpt-file ()
  "Run simrpt on current file"
  (interactive)
  (compile (concat "simrpt --emacs " (buffer-file-name))))

(global-set-key (kbd "C-c C-s") 'simrpt-file)

;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------
;; comfirm emacs exit
(setq confirm-kill-emacs 'yes-or-no-p)

(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c f") 'find-file-at-point)

;; F5 to revert buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

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
(setq undo-outer-limit 1000000000)

;; regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; misc key-bindings
(global-set-key (kbd "C-c /") 'comment-region)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-enabled-themes (quote (doom-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "aa955602112064ea7bd9ed258d06cdce578f1f95cdb56386410a42d2c8e4c7ec" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "e461bbe84cc9461e2d8153d1e310713482bd2451c3207a8e184bc11f9ce39a97" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#eee8d5")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages (quote (helm)))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
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
(put 'downcase-region 'disabled nil)
