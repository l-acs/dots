(menu-bar-mode -1)



(require 'package)


(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(setq package-list
      '(centaur-tabs cider emojify helm kotlin-mode lua-mode magit markdown-mode right-click-context tuareg use-package))


;; (unless package-archive-contents
(package-refresh-contents);)


(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages 'package-list)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq vc-follow-symlinks t)

;;enable right-click menu
(right-click-context-mode 1)

;;update files automatically when changed on disk
(global-auto-revert-mode t)



;; ido mode
;; (ido-mode t)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)


;; matching parentheses
(show-paren-mode 1)

;;compile with pdflatex by default
(setq latex-run-command "pdflatex")


;; emoji globally 🌍
(add-hook 'after-init-hook #'global-emojify-mode)



;tabbed mode?
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward))



(use-package helm
  :ensure t
  :config (helm-mode 1))



;<works>
;; (use-package ewal
;;   :init (setq ewal-use-built-in-always-p nil
;;               ewal-use-built-in-on-failure-p t
;;               ewal-built-in-palette "sexy-material"))
;</works>


;; (use-package ewal-spacemacs-themes     
;;   :init (progn
;;           (setq spacemacs-theme-underline-parens t
;;                 my:rice:font (font-spec
;;                               :family "Source Code Pro"
;;                               :weight 'semi-bold
;;                               :size 11.0))
;;           (show-paren-mode +1)
;;           (global-hl-line-mode)
;;           (set-frame-font my:rice:font nil t)
;;           (add-to-list  'default-frame-alist
;;                         `(font . ,(font-xlfd-name my:rice:font))))
;;   :config (progn
;;             (load-theme 'ewal-spacemacs-modern t)
;;             (enable-theme 'ewal-spacemacs-modern)))
;; (use-package ewal-evil-cursors
;;   :after (ewal-spacemacs-themes)
;;   :config (ewal-evil-cursors-get-colors
;;            :apply t :spaceline t))
;; (use-package spaceline
;;   :after (ewal-evil-cursors winum)
;;   :init (setq powerline-default-separator nil)
;;   :config (spaceline-spacemacs-theme))



; key bindings: wish me luck!



;(define-key global-map "\C-z" 'undo)
;(define-key global-map "\C-Z" 'redo)
;(define-key global-map "\C-/" 'isearch-forward)
;(define-key global-map "\C-?" 'isearch-backward)

;for muscle memory
(global-unset-key "\C-j")
(global-unset-key "\C-k")
(global-unset-key "\C-n")
(global-unset-key "\C-p")
(global-unset-key "\M-f")
(global-unset-key "\M-b")
(global-unset-key "\C-f")
(global-unset-key "\C-b")
(global-unset-key "\C-a")
(global-unset-key "\C-e")
(global-unset-key "\M-a")
(global-unset-key "\M-v")
(global-unset-key "\C-v")
(global-unset-key "\M-s")
;(global-unset-key "\C-x\C-s")
;(global-unset-key "\C-xu")
;(global-unset-key "\C-xk")


;<known to work>



(define-key global-map "\C-f" 'isearch-forward)
(define-key global-map "\M-f" 'isearch-backward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map "\M-f" 'isearch-repeat-backward)


; text movement
(define-key global-map "\M-k" 'move-end-of-line)
(define-key global-map "\M-j" 'move-beginning-of-line)
(local-unset-key "\C-j")
(local-unset-key "\C-k")
(define-key global-map "\C-l" 'forward-char)
(define-key global-map "\C-h" 'backward-char)

(define-key global-map "\C-j" 'next-line)
(define-key global-map "\C-k" 'previous-line)
(define-key global-map "\M-l" 'forward-word)
(define-key global-map "\M-h" 'backward-word)



(define-key global-map "\M-g" 'beginning-of-buffer)
(define-key global-map "\M-G" 'end-of-buffer)





;text manipulation

(define-key global-map "\C-a" 'mark-whole-buffer)


(define-key global-map "\M-x" 'kill-region)
(define-key global-map "\M-c" 'kill-ring-save)
(define-key global-map "\M-v" 'yank)


(define-key global-map "\C-d" 'delete-char)
(define-key global-map "\M-d" 'kill-line)
(define-key global-map "\M-w" 'kill-word)
(define-key global-map "\C-w" 'kill-word)


;general operations & IO
(define-key global-map "\C-b" 'switch-to-buffer)
(define-key global-map "\C-e" 'eval-last-sexp)

(define-key global-map "\C-s" 'save-buffer)

(define-key global-map "\M-e" 'execute-extended-command)

; windows
(define-key global-map (kbd "<C-tab>") 'other-window)

; tabbed life
(define-key global-map "\C-t" 'find-file)

; emoji
(define-key global-map "\M-E" 'emojify-insert-emoji)


;testing
(define-key global-map "\C-q" 'keyboard-quit)

;</known to work>




;; paragraph?			
;; backward paragraph?


;; org
(define-key global-map  (kbd "C-c l") 'org-store-link)

