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
      '(centaur-tabs cider company dictionary docker-compose-mode dockerfile-mode emojify ess helm indium kotlin-mode lua-mode magit markdown-mode org-journal popwin projectile tuareg undo-tree use-package))


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
 '(centaur-tabs-background-color "blue")
 '(emojify-emoji-styles (quote (github unicode)))
 '(helm-completion-style (quote emacs))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(message-kill-buffer-query nil)
 '(org-agenda-files
   (split-string
    (replace-regexp-in-string "
\\'" ""
(shell-command-to-string "find ~/Documents/gtd -mindepth 1 -type d"))
    "
"))
 '(org-log-done (quote time))
 '(org-support-shift-select t)
 '(package-selected-packages 'package-list)

 '(parens-require-spaces nil)
 '(safe-local-variable-values
   (quote
    ((cider-shadow-default-options . ":app")
     (cider-default-cljs-repl . shadow))))
 '(sql-product (quote mysql))
 '(text-scale-mode-step 1.05)
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

;; theme
(load-theme 'leuven)

;; typing overwrites selection if active (http://pragmaticemacs.com/emacs/overwrite-selected-text/)
(delete-selection-mode t)

;; sessions
(setq desktop-save-mode t)
;; A file which saves the desktop has a lock while a session runs. A
;; session will always be running whenever the daemon still is (and
;; the lock will thus never be automatically released).
(setq desktop-load-locked-desktop t)
;; When not t: a desktop won't load automatically if the saved session
;; still has its lock; Emacs cannot restore the session of an
;; unstopped daemon——starting again, it will be unable to prompt
;; interactively whether to load a locked session.


;; hide finished todos from agenda
(setq org-agenda-skip-scheduled-if-done t)

;; change up some keywords
(setq org-todo-keywords
      '((sequence "todo:" "doing:" "later:" "done:")
	(sequence "⭕" "🔵" "⏩" "✅")
	(sequence "try:" "maybe:" "waiting:")
	(sequence "clarify:" "to capture:")))


;; lower default priority, I guess
;; it's not actually supposed to let you do this, but it does
(setq org-default-priority ?D) ;; 2021-09-27

;; make it so that if you do org-agenda-priority-down on an entry w/ no priority, it will start w/ #A
(setq org-priority-start-cycle-with-default nil) ;; 2021-09-27


;; use org-indent-mode
;; and org keybinds

(setq org-unset
      (append
       (list "\M-e" "\M-h" "\C-j" "\C-k" "\C-a" "\C-e")
       (mapcar 'kbd
	       (list "M-<right>" "M-S-<right>"
		     "M-<left>" "M-S-<left>" "M-{" "M-}" "C-<up>" "C-<down>" "C-<tab>"))))

(setq org-bindings
      (list
       "\M-[" 'org-metaleft "\M-{" 'org-shiftmetaleft
       "\M-[" 'org-metaleft  "\M-{" 'org-shiftmetaleft
       "\M-]" 'org-metaright "\M-}" 'org-shiftmetaright

       (kbd "C-<up>") 'org-backward-element
       (kbd "C-<down>") 'org-forward-element

       (kbd "\C-c d") 'org-cut-subtree

       "\M-[" 'org-metaleft "\M-{" 'org-shiftmetaleft
       "\M-]" 'org-metaright "\M-}" 'org-shiftmetaright

       (kbd "C-`") 'org-force-cycle-archived
       (kbd "M-`") 'org-todo))

(defun my-org-stuff ()
  "Turn on indent mode, turn on auto-fill mode, unbind some keys, bind some keys"
  (progn
    (org-indent-mode)
    (auto-fill-mode)
    (mapc 'local-unset-key org-unset)
    (mapc (lambda (s)
	(apply 'local-set-key s))
      (seq-partition org-bindings 2))))

(setq org-agenda-unset
     (append
      (list 	"\C-j"
		"\C-k")


       (mapcar 'kbd
	       (list
		;; "M-<right>"
		"M-S-<right>"
		;; "M-<left>"
		"M-S-<left>"
		;; "M-{" "M-}" "C-<up>" "C-<down>" "C-<tab>"
		))))


(setq org-agenda-bindings
      (list
       (kbd "\C-j") 'org-agenda-next-line
       (kbd "\C-k") 'org-agenda-previous-line

       ;; (kbd "\C-c C-<down>") 'org-agenda-priority-down
       ;; (kbd "\C-c C-<up>") 'org-agenda-priority-up

       ;; (kbd "M-S-<left>") 'org-agenda-date-earlier
       ;; (kbd "M-S-<right>") 'org-agenda-date-later

       (kbd "<left>") 'org-agenda-date-earlier
       (kbd "<right>") 'org-agenda-date-later

       "F" 'org-agenda-filter-by-tag

       "?" 'isearch-backward-regexp
       "/" 'isearch-forward-regexp

       "n" 'isearch-repeat-forward
       "N" 'isearch-repeat-backward

       ;; (kbd "\C-c -") 'org-agenda-date-earlier
       ;; (kbd "\C-c =") 'org-agenda-date-later

       "z" 'org-agenda-undo
       "c" 'org-agenda-add-note

       "j" 'org-agenda-next-line
       "k" 'org-agenda-previous-line

       "C" 'org-agenda-recenter
       "L" 'org-agenda-log-mode
       "f" 'org-agenda-follow-mode


       ;; "h" 'org-agenda-earlier
       ;; "l" 'org-agenda-later

       "p" 'org-agenda-priority
       "r" 'org-agenda-redo-all

       ;; (progn ;; not working: "wrong type" or something, fix later
       ;; 	     (org-save-all-org-buffers)
       ;; 	     (org-agenda-redo-all))

       "y" 'org-agenda-goto-today
       "d" 'org-agenda-goto-date
;;       "Y" 'org-agenda-year-view

       "," 'org-agenda-earlier
       "." 'org-agenda-later


       "g" 'beginning-of-buffer
       "G" 'end-of-buffer

       "V" 'org-agenda-toggle-time-grid

       (kbd "<up>") 'org-agenda-priority-up
       (kbd "<down>") 'org-agenda-priority-down

       "S" 'org-agenda-schedule



))

;; (defun my-org-agenda-stuff ()
;;   "Unbind some keys, bind some keys"
;; ;;  "Turn on indent mode, unbind some keys, bind some keys"
;;   (progn
;;     ;; (org-indent-mode)
;;     (mapc 'local-unset-key org-agenda-unset) ;; not working as expected
;;     (mapc (lambda (s)
;; 	(apply 'local-set-key s))
;;       (seq-partition org-agenda-bindings 2))))

(defun bind-keypair (kp)
  "Bind a list of key and fun"
  (apply 'local-set-key kp))
;; seems to work:
;; (bind-keypair (list (kbd "\C-c RET") 'org-capture))

(defun bind-keylist (kl)
  "Sequentially bind a list of keypairs"
  (mapc
   'bind-keypair kl))

(add-hook 'org-mode-hook 'my-org-stuff)

(add-hook 'org-agenda-mode-hook
	  (lambda () (bind-keylist
		      (seq-partition org-agenda-bindings 2))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (progn
	      (local-unset-key "{")
	      (local-unset-key "}"))))
(add-hook 'tex-mode-hook
	  (lambda ()
	    (local-unset-key "\C-j")))


;; enable downcase-region
(put 'downcase-region 'disabled nil)


;; tabbed mode?
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  ("<C-tab>" . centaur-tabs-backward)
  ("<C-S-isolefttab>" . centaur-tabs-forward)
)


(use-package helm
  :ensure t
  :config (helm-mode 1))

;; add eval line functionality with Python
;; from https://emacs.stackexchange.com/questions/52108/evaluate-single-line-of-python-code-from-emacs-org-babel

;; Add a function to send a single line to the Python console
(defun python-shell-send-line ()
  (interactive)
  (save-mark-and-excursion
    (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)
    (python-shell-send-region
     (region-beginning)
     (region-end))))

;; Create shortcut
(with-eval-after-load "python" (define-key python-mode-map
                 (kbd "C-c C-j")
                 'python-shell-send-line))



;; general key bindings

(defun def-keymap (keymap pairlist)
  ""
  (mapc (lambda (l)
	  (define-key keymap (car l) (car (cdr l))))
       pairlist))

;; for muscle memory
(setq global-unsets '("\C-j" "\C-k" "\C-n" "\C-p" "\M-f" "\M-b" "\C-f" "\C-b" "\C-a" "\C-e" "\M-a" "\M-v" "\C-v" "\C-y" "\M-y" "\C-r" "\M-r" "\M-s" "\C-t" "\C-q" "\M-<" "\M->" "(" "{" "<" "~" "`" "\"" "_"))
(mapc 'global-unset-key global-unsets)

(def-keymap ctl-x-map
  (list
   '("u" nil) '("k" nil) '("\C-w" nil) '("\C-k" nil)))


(defun insert-wrap (open close &optional arg)
  "Enclose following ARG sexps in parentheses.
Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment."
  (interactive "P")
  ;;  (insert-pair arg ?\( ?\)))
  ;; the question mark means character literals
  (insert-pair arg open close))

(defun wrap-if-region (&optional arg open close)
  ;; taken from insert-pair
  ;; if there's a region, wrap in open ... close
  ;; otherwise, insert open
  "Enclose following ARG sexps in a pair of OPEN and CLOSE characters.
Leave point after the first character.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert characters
and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

If arguments OPEN and CLOSE are nil, the character pair is found
from the variable `insert-pair-alist' according to the last input
character with or without modifiers.  If no character pair is
found in the variable `insert-pair-alist', then the last input
character is inserted ARG times.

This command assumes point is not in a string or comment."
  (interactive "P")
  (if (not (and open close))
      (let ((pair (or (assq last-command-event insert-pair-alist)
                      (assq (event-basic-type last-command-event)
                            insert-pair-alist))))
        (if pair
            (if (nth 2 pair)
                (setq open (nth 1 pair) close (nth 2 pair))
              (setq open (nth 0 pair) close (nth 1 pair))))))
  (if (and open close)
      (if (and transient-mark-mode mark-active)
	  (progn
	    (save-excursion
	      (goto-char (region-end))
	      (insert close))
	    (goto-char (region-beginning))
	    (insert open))
	(insert open))))

(setq insert-pair-alist '((40 41)
			  (91 93)
			  (123 125)
			  (60 62)
			  (34 34)
			  (39 39)
			  (96 96)   ; backticks
			  (126 126) ; tildes
			  (95 95))) ; underscores

(defun wrap-curly (&optional arg)
  (interactive "P")
  (insert-wrap ?\{ ?\}))
  ;; (wrap-if-region ?\{ ?\}))

(defun wrap-bracket (&optional arg)
  (interactive "P")
  (insert-wrap ?\[ ?\]))
;;  (wrap-if-region ?\[ ?\]))

(defun wrap-angle (&optional arg)
  (interactive "P")
  (wrap-if-region ?\< ?\>))

(defun wrap-quote (&optional arg)
  (interactive "P")
  (wrap-if-region ?\" ?\"))

(defun wrap-tilde (&optional arg)
  (interactive "P")
  (wrap-if-region ?~ ?~))

(defun wrap-backtick (&optional arg)
  (interactive "P")
  (wrap-if-region ?\` ?\`))

(defun wrap-underscore (&optional arg)
  (interactive "P")
  (wrap-if-region ?_ ?_))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun eval-replace-last-sexp ()
  "Eval sexp and replace it inline"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun eval-print-region (start end &optional printflag read-function)
  "I mean, it works. Weird line separation going on."
  (interactive "r")
  (save-excursion
    (eval-region start end (copy-marker (point)) read-function)))


(defun agenda ()
  "Open Org Agenda in day view"
  (interactive)
  (let '(org-agenda-span 'day)
    (org-agenda-list)))

;; macros
(fset 'new-empty-line-above
      [?\M-h return up])

(fset 'new-empty-line-below
      [?\M-l return])

(fset 'unfill-line
   "\354\C-d\334\354")

;; searching
(def-keymap isearch-mode-map
  (list
   '("\C-f" isearch-repeat-forward) '("\M-f" isearch-repeat-backward)))

(def-keymap global-map
  (list
   '("\C-f" isearch-forward-regexp) '("\M-f" isearch-backward-regexp)
   
   ;; text movement
   '("\M-h" move-beginning-of-line) '("\M-l" move-end-of-line)
   '("\C-l" forward-char) '("\C-h" backward-char)
   '("\C-j" next-line) '("\C-k" previous-line)
   '("\M-j" backward-word) '("\M-k" forward-word)
   '("\M-g" beginning-of-buffer) '("\M-G" end-of-buffer)
   '("\M-o" new-empty-line-below) '("\M-O" new-empty-line-above)

   ;; text manipulation
   '("\C-a" mark-whole-buffer)
   '("\M-x" kill-region) '("\M-c" kill-ring-save)
   '("\M-v" yank) '("\M-V" yank-pop)
   '("\C-d" delete-char) '("\M-w" kill-word)
   '("\M-d" kill-line) '("\M-D" kill-whole-line)
   '("\C-z" undo)
   '("\M-9" insert-parentheses)
   '("(" insert-parentheses)
   '("{" wrap-curly)
   '("[" wrap-bracket)
   '("<" wrap-angle)
   '("\"" wrap-quote)
   '("~" wrap-tilde)
   '("`" wrap-backtick)
   '("_" wrap-underscore)
   '("\M-0" forward-or-backward-sexp)
   '("\M-Q" quoted-insert)
   '("\M-\\" just-one-space)
   '("\M-U" downcase-word)


   ;; elisp
   '("\C-e" eval-replace-last-sexp) (list (kbd "C-M-e") 'eval-print-last-sexp)
   (list (kbd "C-M-r") 'eval-print-region)
   '("\M-e" execute-extended-command)
   '("\M-," xref-find-definitions)
   '("\M-<" xref-pop-marker-stack)
   '("\M-." repeat)
   '("\M->" repeat-complex-command)

   ;; windows, buffers, and files
   ;; (list (kbd "<C-tab>") 'other-window)
   '("\M-r" other-window)
   ;; (list (kbd "<C-tab>") 'previous-buffer)
   ;; (list (kbd "<C-S-isolefttab>") 'next-buffer)
   '("\M-b" previous-buffer) '("\C-b" switch-to-buffer)

   '("\C-w" kill-current-buffer)
   (list (kbd "C-x C-w") 'kill-buffer)

   '("\M-W" write-file)
   '("\C-t" find-file) (list (kbd "C-x t") 'find-file-at-point)
   '("\C-s" save-buffer) '("\C-n" make-frame-command)

   ;; emoji
   '("\M-E" emojify-insert-emoji)
   
   ;; centaur
   (list (kbd "C-x <right>") 'centaur-tabs-forward-tab)

   ;; org
   (list (kbd "C-c l") 'org-store-link)
   (list (kbd "M-SPC") 'agenda)

   ;; emacs
   '("\C-q" delete-frame)))

;; give escape expected behavior
(def-keymap key-translation-map
  (list
   (list (kbd "ESC") (kbd "C-g"))
   (list (kbd "C-<escape>") (kbd "ESC"))))


(set-face-attribute 'default nil :height 250)



;; 2021-07-13: some env var stuff, not sure if good
;; 2021-07-13: it might do well to have these instead be in another elisp file -- maybe one not `stow`ed -- to be added to the load path
(setenv "PATH" (concat (getenv "PATH") ":/home/l-acs/.scripts:/home/l-acs/.scripts/*")) ;; the globbing doesn't work
(setenv "TD_DIR" "/home/l-acs/Documents/gtd/td") ;; why is this here? maybe for `td` w/in the Emacs shell?

;; how do I source .zshrc "into" the PATH?

(desktop-read (concat (getenv "HOME") "/.emacs.d/"))
;; 2021-09-27: this initially seemed to break the daemon somehow (and
;; yet running emacsclient a second time seemed to do just fine) but
;; it seems as though the problem's gone away



;; from https://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t			; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.emacs.d/saves/"))	; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)			; use versioned backups
;; 2022-01-05: try to not clutter everything with ~s everywhere


;; 2022-01-05: make C-S-tab tab work
(define-key function-key-map [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map [(meta control shift iso-lefttab)] [(meta control shift tab)])

;; 2022-01-05: try to fix helm cluttering
(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)


(let '(org-agenda-span 'day) (org-agenda-list)) ;; start org-agenda, temporarily setting `org-agenda-span` to day view to do so

;; N.B.
;; M-x name-last-kbd-macro
;; M-x insert-kbd-macro
