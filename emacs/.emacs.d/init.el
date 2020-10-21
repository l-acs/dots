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
(package-refresh-contents)
;; )


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
 '(custom-enabled-themes '(deeper-blue))
 '(emojify-emoji-styles '(github unicode))
 '(helm-completion-style 'emacs)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(message-kill-buffer-query nil)
 '(org-support-shift-select t)
 '(org-todo-keywords '((sequence "⭕" "🔵" "▶" "⏩" "✅")))
 '(package-selected-packages 'package-list)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))


(custom-set-faces

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
;; and org keybinds

(setq org-unset
      (append
       (list "\M-e" "\M-h" "\C-j")
       (mapcar 'kbd
	       (list "M-<right>" "M-S-<right>" "M-<left>" "M-S-<left>" "M-{" "M-}" "C-<up>" "C-<down>" "C-<tab>"))))

(setq org-bindings
      (list
       "\M-[" 'org-metaleft "\M-{" 'org-shiftmetaleft
       "\M-[" 'org-metaleft  "\M-{" 'org-shiftmetaleft
       "\M-]" 'org-metaright "\M-}" 'org-shiftmetaright
       (kbd "C-<up>") 'org-forward-element
       (kbd "C-<down>") 'org-backward-element
       (kbd "C-`") 'org-force-cycle-archived
       "\M-[" 'org-metaleft "\M-{" 'org-shiftmetaleft
       "\M-]" 'org-metaright "\M-}" 'org-shiftmetaright
       (kbd "C-<up>") 'org-forward-element
       (kbd "C-<down>") 'org-backward-element
       (kbd "C-`") 'org-force-cycle-archived))

(defun my-org-stuff ()
  "Turn on indent mode, unbind some keys, bind some keys"
  (progn 
    (org-indent-mode)
    (mapc 'local-unset-key org-unset)
    (mapc (lambda (s)
	(apply 'local-set-key s))
      (seq-partition org-bindings 2))))

(add-hook 'org-mode-hook 'my-org-stuff)


;; enable downcase-region
(put 'downcase-region 'disabled nil)


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

(defun def-keymap (keymap pairlist)
  ""
  (mapc (lambda (l)
	  (define-key keymap (car l) (car (cdr l))))
       pairlist))

;; for muscle memory
(setq global-unsets '("\C-j" "\C-k" "\C-n" "\C-p" "\M-f" "\M-b" "\C-f" "\C-b" "\C-a" "\C-e" "\M-a" "\M-v" "\C-v" "\M-s" "\C-t"))
(mapc 'global-unset-key global-unsets)

(def-keymap ctl-x-map
  (list
   '("u" nil) '("k" nil)))


(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; searching
(def-keymap isearch-mode-map
  (list
   '("\C-f" isearch-repeat-forward) '("\M-f" isearch-repeat-backward)))

(def-keymap global-map
  (list
   '("\C-f" isearch-forward-regexp) '("\M-f" isearch-forward-regexp)
   
   ;; text movement
   '("\M-k" move-end-of-line) '("\M-j" move-beginning-of-line)
   '("\C-l" forward-char) '("\C-h" backward-char)
   '("\C-j" next-line) '("\C-k" previous-line)
   '("\M-l" forward-word) '("\M-h" backward-word)
   '("\M-g" beginning-of-buffer) '("\M-G" end-of-buffer)

   ;; text manipulation
   '("\C-a" mark-whole-buffer) '("\M-x" kill-region)
   '("\M-c" kill-ring-save) '("\M-v" yank)
   '("\C-d" delete-char) '("\M-d" kill-line)
   '("\M-w" kill-word) '("\C-w" kill-word)
   '("\M-0" forward-or-backward-sexp)
   '("\C-z" undo)

   ;; elisp
   '("\C-e" eval-last-sexp) '("\M-e" execute-extended-command)

   ;; windows, buffers, and files
   (list (kbd "<C-tab>") 'other-window) (list (kbd "C-x k") 'kill-current-buffer)
   (list (kbd "C-x C-k") 'kill-buffer) '("\C-b" switch-to-buffer)
   '("\C-s" save-buffer) '("\C-t" find-file)

   ;; emoji
   '("\M-E" emojify-insert-emoji)
   
   ;; centaur
   (list (kbd "C-x <right>") 'centaur-tabs-forward-tab)

   ;; org
   (list (kbd "C-c l") 'org-store-link)))

;; give escape expected behavior
(def-keymap key-translation-map
  (list
   (list (kbd "ESC") (kbd "C-g"))
   (list (kbd "C-<escape>") (kbd "ESC"))))




(set-face-attribute 'default nil :height 140)
