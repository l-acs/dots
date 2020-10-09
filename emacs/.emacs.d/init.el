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
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(setq package-list
      '(centaur-tabs cider emojify ess helm kotlin-mode lua-mode magit markdown-mode tuareg undo-tree use-package))


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
 '(message-kill-buffer-query nil)
 '(org-support-shift-select t)
 '(org-todo-keywords '((sequence "⭕" "🔵" "▶" "⏩" "✅")))
 '(package-selected-packages 'package-list)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; use org in *scratch*
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "Welcome to GNU Emacs!\n\n")

;; put newlines at end of files
(setq require-final-newline t)

;; follow symlinks
(setq vc-follow-symlinks t)


;; update files automatically when changed on disk
(global-auto-revert-mode t)


;; matching parentheses
(show-paren-mode 1)

;; compile with pdflatex by default
(setq latex-run-command "pdflatex")


;; emoji globally 🌍
(add-hook 'after-init-hook #'global-emojify-mode)


;; use org-indent-mode
(add-hook 'org-mode-hook 'org-indent-mode)



;; tabbed mode?
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







;; key bindings


;; for muscle memory
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



;<known to work>

(define-key global-map "\C-f" 'isearch-forward)
(define-key global-map "\M-f" 'isearch-backward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map "\M-f" 'isearch-repeat-backward)


;; text movement
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





;; text manipulation

(define-key global-map "\C-a" 'mark-whole-buffer)

(define-key global-map "\M-x" 'kill-region)
(define-key global-map "\M-c" 'kill-ring-save)
(define-key global-map "\M-v" 'yank)


(define-key global-map "\C-d" 'delete-char)
(define-key global-map "\M-d" 'kill-line)
(define-key global-map "\M-w" 'kill-word)
(define-key global-map "\C-w" 'kill-word)

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(define-key global-map "\M-0" 'forward-or-backward-sexp)

;general operations & IO
(define-key global-map "\C-b" 'switch-to-buffer)
(define-key global-map "\C-e" 'eval-last-sexp)

(define-key global-map "\C-s" 'save-buffer)

(define-key global-map "\M-e" 'execute-extended-command)

; windows
(define-key global-map (kbd "<C-tab>") 'other-window)


;; tabbed life
(define-key global-map "\C-t" 'find-file)

;; emoji
(define-key global-map "\M-E" 'emojify-insert-emoji)

;; give escape expected behavior
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))

;</known to work>


;; org
(define-key global-map  (kbd "C-c l") 'org-store-link)

(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "M-h") nil)
(define-key org-mode-map (kbd "C-j") nil)

(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "M-S-<right>") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-S-<left>") nil)
(define-key org-mode-map (kbd "M-{") nil)
(define-key org-mode-map (kbd "M-}") nil)
(define-key org-mode-map (kbd "C-<up>") nil)
(define-key org-mode-map (kbd "C-<down>") nil)

(define-key org-mode-map (kbd "C-<tab>") nil)

(define-key org-mode-map "\M-[" 'org-metaleft)
(define-key org-mode-map "\M-{" 'org-shiftmetaleft)
(define-key org-mode-map "\M-]" 'org-metaright)
(define-key org-mode-map "\M-}" 'org-shiftmetaright)

(define-key org-mode-map (kbd "C-<up>") 'org-forward-element)
(define-key org-mode-map (kbd "C-<down>") 'org-backward-element)

(define-key org-mode-map (kbd "C-`") 'org-force-cycle-archived)





