;; ====================
;; third-party packages
;; ====================


(require 'package)


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))


(package-initialize)


(let ((refreshed)
      (packages '(zenburn-theme
                  cider
                  slime
                  groovy-mode
                  ox-textile
                  htmlize
                  clojure-mode
                  powershell
                  smex)))
  (dolist (p packages)
    (unless (package-installed-p p)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install p))))


;; ======
;; system
;; ======


;; 'are we on windows?'-shortcut


(setq system-type-is-windows (string-equal system-type "windows-nt"))


;; enable useful APIs


(progn (require 'cl-lib)
       (require 'subr-x))


;; encoding


(reset-language-environment)


(set-coding-system-priority 'cp1251-dos)


(prefer-coding-system 'utf-8-unix)


(setq default-input-method 'russian-computer)


;; place customization data to separate file (do not edit it by hand)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(when (file-exists-p custom-file)
  (load-file custom-file))


;; navigate to home directory on startup


(when window-system
  (cd (or (getenv "USERPROFILE") "~")))


;; automatically kill subprocesses on emacs shutdown


(setq confirm-kill-processes nil)


;; built-in history facilities


(savehist-mode 1)


(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


(setq desktop-save-mode 1)


;; enable unix'y things from MSYS2


(when system-type-is-windows
  (let ((msys "C:/tools/msys64"))
    (if (file-exists-p msys)
        (progn (setenv "PATH"
                       (format "%s\\usr\\bin;%s"
                               (replace-regexp-in-string "/" "\\\\" msys)
                               (getenv "PATH")))
               (add-to-list 'exec-path (format "%s/usr/bin" msys))
               (setq shell-file-name "bash"))
      (warn "msys2 not found. Expected location is %s" msys))))


;; ==================
;; global keybindings
;; ==================


;; unset all C-dight / M-dight combos


(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))


;; starting REPLs


(progn
  (define-prefix-command 'repls-map)
  (global-set-key (kbd "M-l") 'repls-map)
  (define-key 'repls-map (kbd "i") 'ielm)
  (define-key 'repls-map (kbd "s") 'slime)
  (define-key 'repls-map (kbd "l") 'run-shell)
  (define-key 'repls-map (kbd "p") 'run-python-with-venv)
  (define-key 'repls-map (kbd "r") 'run-racket)
  (define-key 'repls-map (kbd "q") 'sql-connect)
  (progn (define-prefix-command 'run-cider-map)
         (define-key 'repls-map (kbd "j") 'run-cider-map)
         (define-key 'run-cider-map (kbd "k") 'cider-connect)
         (define-key 'run-cider-map (kbd "j") 'cider-jack-in)))


;; extending global search map


(progn
  (define-key search-map (kbd "r") 'find-dired)
  (define-key search-map (kbd "g") 'rgrep)
  (define-key search-map (kbd "t") 'translate-en-ru-online))


;; transforming text


(progn
  (define-prefix-command 'text-transform-map)
  (global-set-key (kbd "M-c") 'text-transform-map)
  (define-key 'text-transform-map (kbd "c") 'upcase-char)
  (define-key 'text-transform-map (kbd "s") 'replace-string)
  (define-key 'text-transform-map (kbd "u") 'upcase-dwim)
  (define-key 'text-transform-map (kbd "d") 'downcase-dwim)
  (define-key 'text-transform-map (kbd "M-c") 'duplicate-line)
  (define-key 'text-transform-map (kbd "/") 'invert-slashes)
  (define-key 'text-transform-map (kbd "j") 'join-region)
  (define-key 'text-transform-map (kbd "b") 'break-line)
  (define-key 'text-transform-map (kbd "f") 'flush-lines)
  (define-key 'text-transform-map (kbd "k") 'keep-lines)
  (define-key 'text-transform-map (kbd ".") 'wrap-with-tags)
  (define-key 'text-transform-map (kbd "\"") '(lambda () (interactive) (wrap-with-text "\"" "\"")))
  (define-key 'text-transform-map (kbd "'") '(lambda () (interactive) (wrap-with-text "'" "'")))
  (define-key 'text-transform-map (kbd "[") '(lambda () (interactive) (wrap-with-text "[" "]")))
  (define-key 'text-transform-map (kbd "{") '(lambda () (interactive) (wrap-with-text "{" "}")))
  (define-key 'text-transform-map (kbd "(") '(lambda () (interactive) (wrap-with-text "(" ")"))))


;; evaluating emacs lisp


(progn
  (define-prefix-command 'eval-elisp-map)
  (global-set-key (kbd "C-c e") 'eval-elisp-map)
  (define-key 'eval-elisp-map (kbd "r") 'eval-region)
  (define-key 'eval-elisp-map (kbd "b") 'eval-buffer))


;; misc


(let ((bindings
       '(("M-x" smex)
         ("M-=" count-words)
         ("C-x C-b" ibuffer)
         ("C-c l" hl-line-mode)
         ("C-c p" copy-file-name-to-clipboard)
         ("C-c j" org-clock-goto)
         ("M-k" kill-whole-line)
         ("C-x u" insert-char)
         ("M-q" hippie-expand)
         ("M-/" hippie-expand-all)
         ("C-v" scroll-up-5-lines)
         ("M-v" scroll-down-5-lines)
         ("C-x C-p" fill-paragraph)
         ("M-i" reindent-region)
         ("M-u" force-revert-buffer)
         ("C-c n" rename-buffer)
         ("C-c h" hexl-mode)
         ("C-c a" org-agenda)
         ("C-c c" org-capture)
         ("C-c s" ssh-tunnel)
         ("C-c m" compile)
         ("M-o" other-window)
         ("C-1" delete-other-windows)
         ("C-2" split-window-below)
         ("C-3" split-window-right)
         ("C-0" delete-window))))
  (dolist (binding bindings)
    (global-unset-key (kbd (car binding)))
    (global-set-key
     (kbd (car binding))
     (cadr binding))))


;; =============
;; look and feel
;; =============


;; remove "welcome" messages on startup


(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;; replace annoying confirmations with less annoying


(fset 'yes-or-no-p 'y-or-n-p)


(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;; turn off bell ringing


(setq ring-bell-function 'ignore)


;; show line numbers


(global-display-line-numbers-mode t)


;; no line-wrap indicators


(when fringe-indicator-alist
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        '(nil nil)))


(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))


;; no toolbar


(tool-bar-mode -1)


;; show-paren mode


(show-paren-mode)


(unless window-system
  (set-face-attribute 'show-paren-match nil :background "magenta")
  (set-face-attribute 'show-paren-mismatch nil :background "red"))


;; fonts


(defun font-available-p (font-name)
  (not (null (member font-name (font-family-list)))))


(defun apply-font (my-font)
  (interactive
   (list (ido-completing-read
          "Font face: "
          (cl-remove-duplicates (font-family-list) :test #'equal))))
  (set-face-attribute 'default nil :font my-font))


(when (font-available-p "Consolas")
  (apply-font "Consolas-10"))


;; color themes


(defun apply-color-theme (theme)
  "Enchanced version of load-theme.
   Make use of IDO and disable any of previously loaded themes before turning on a new one"
  (interactive
   (list (intern (ido-completing-read
                  "theme: "
                  (mapcar #'symbol-name
                          (custom-available-themes))))))
  (when window-system
    (dolist (theme (custom-available-themes))
      (disable-theme theme))
    (load-theme theme)))


;; default color theme


(apply-color-theme 'dichromacy)


;; better modeline


;;;; get region lines/words/chars counts


(defun count-lwc ()
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start)))
    (propertize (format "[lines:%d words:%d chars:%d]" lines words chars)
                'face 'hi-green)))


(require 'hi-lock)


(setq-default mode-line-format
              `((:eval (format "%s%s "
                               (symbol-name buffer-file-coding-system)
                               (if current-input-method-title
                                   (concat " " (propertize (format "[%s]" current-input-method-title) 'face 'hi-blue-b))
                                 "")))
                mode-line-client
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                "   "
                (:eval (when (use-region-p) (count-lwc)))
                (:eval (format " (l:%s c:%s p:%s)"
                               (propertize "%l" 'face 'font-lock-builtin-face)
                               (propertize "%C" 'face 'font-lock-builtin-face)
                               (propertize (number-to-string (point)) 'face 'font-lock-builtin-face)))
                (vc-mode vc-mode)
                "  " mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))


;; =====
;; files
;; =====


;; no backups


(setq auto-save-default nil)


(setq make-backup-files nil)


(setq auto-save-list-file-name nil)


;; reverting file-related buffers


(global-auto-revert-mode t)


(when system-type-is-windows (setq auto-revert-use-notify nil))


(setq revert-without-query '(".*"))


(defun force-revert-buffer ()
  (interactive)
  (message "Force revert buffer '%s'" (buffer-name))
  (revert-buffer nil t t))


;; open file(s) in external app


(defun open-in-external-app (&optional file-name)
  (interactive)
  (let ((open-file
         (cond (system-type-is-windows
                (lambda (f) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" f t t))))
               ((string-equal system-type "darwin")
                (lambda (f) (shell-command (concat "open " (shell-quote-argument f)))))
               ((string-equal system-type "gnu/linux")
                (lambda (f) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" f)))))))
    (funcall open-file file-name)))


;; dired


(require 'ls-lisp)


(setq ls-lisp-format-time-list
      '("%d.%m.%Y %H:%M"
        "%d.%m.%Y %H:%M")
      ls-lisp-use-localized-time-format t
      ls-lisp-dirs-first t)


(setq dired-listing-switches (concat "-alh" (unless system-type-is-windows
                                              " --group-directories-first"))
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun dired-open-in-external-app ()
  (interactive)
  (mapc #'open-in-external-app (dired-get-marked-files)))


(with-eval-after-load 'dired
  (define-key dired-mode-map
    (kbd "C-c C-o")
    'dired-open-in-external-app)
  (define-key dired-mode-map
    (kbd "`")
    'dired-hide-details-mode))


(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;; copy full names of files to clipboard


(defun copy-file-name-to-clipboard ()
  "Copy the current file name(s) to the clipboard"
  (interactive)
  (let ((names (if (equal major-mode 'dired-mode)
                   (string-join (dired-get-marked-files) "\n")
                 (buffer-file-name))))
    (when names
      (kill-new names)
      (message "Name copied to clipboard: %s" names))))


;; ===========
;; text editor
;; ===========


;; auto-completion with TAB


(setq tab-always-indent 'complete)


(add-to-list 'completion-styles 'initials t)


;; use spaces for indentation


(setq-default indent-tabs-mode nil)


;; overwrite selected text


(delete-selection-mode t)


;; reindent / cleanup selected region or whole buffer


(defun reindent-region (start end)
  "Reindent selected region, untabify it, remove trailing whitespaces"
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (untabify start end)
  (delete-trailing-whitespace start end)
  (indent-region start end))


;; change \ to / and vice versa


(defun invert-slashes (start end)
  "Change \\ to / and vice versa in selected region or in whole buffer"
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[/\\]" end t)
      (replace-match
       (if (equal (match-string-no-properties 0) "/")
           "\\\\" "/")))))


(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        (while (> count 0)
          (newline)
          (insert line)
          (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg))


(defun join-region ()
  (interactive)
  (when (region-active-p)
    (let ((separator (read-string "Join region with: "
                                  (and (boundp 'break-line-separator)
                                       break-line-separator)))
          (text (buffer-substring
                 (region-beginning)
                 (region-end))))
      (setq text (split-string text "\n" t " *"))
      (setq text (string-join text separator))
      (delete-active-region)
      (insert text)
      (setq break-line-separator separator))))


(defun break-line ()
  (interactive)
  (let ((separator (read-string "Break line with: "
                                (and (boundp 'break-line-separator)
                                     break-line-separator)))
        (text (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (setq text (split-string text separator t))
    (setq text (string-join text "\n"))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert text)
    (setq break-line-separator separator)))


(defun wrap-with-text (b1 b2)
  "Wrap current word (or region) with given bracket-like strings
   (e.g. brackets/quotes/apostrophes/parens etc.)"
  (if (region-active-p)
      (let ((s (region-beginning))
            (e (region-end)))
        (save-excursion
          (goto-char e)
          (insert b2)
          (goto-char s)
          (insert b1)))
    (save-excursion
      (forward-word)
      (backward-word)
      (insert b1)
      (forward-word)
      (insert b2))))


;; slower scrolling


(defun scroll-down-5-lines ()
  (interactive)
  (scroll-down-command 5))


(defun scroll-up-5-lines ()
  (interactive)
  (scroll-up-command 5))


;; =======
;; ibuffer
;; =======


(setq ibuffer-expert t)


(setq ibuffer-default-sorting-mode 'alphabetic)


(setq ibuffer-show-empty-filter-groups nil)


(defun ibuffer-custom-setup ()
  (ibuffer-auto-mode 1)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Shell" (or (mode . shell-mode)
                        (mode . eshell-mode)
                        (name . "^\\*Shell .*$")))
           ("Text" (and (not (name . "^\\*.*\\*$"))
                        (not (mode . org-mode))
                        (or (derived-mode . text-mode)
                            (mode . fundamental-mode)
                            (derived-mode . conf-mode))))
           ("Org-mode" (or (mode . org-mode)
                           (name . "^\\*Org .*$")))
           ("SQL" (or (mode . sql-mode)
                      (mode . sql-interactive-mode)))
           ("C" (mode . c-mode))
           ("Java" (mode . java-mode))
           ("Groovy" (or (mode . groovy-mode)
                         (mode . inferior-groovy-mode)))
           ("Shell script" (or (mode . sh-mode)
                               (mode . bat-mode)))
           ("PowerShell" (or (mode . powershell-mode)
                             (name . "^\\*posh.*$")))
           ("Python" (or (mode . python-mode)
                         (mode . inferior-python-mode)))
           ("Javascript" (mode . js-mode))
           ("Common Lisp" (or (name . "^\\*inferior-lisp\\*$")
                              (mode . lisp-mode)
                              (name . "^\\*slime-.**$")))
           ("Scheme" (or (mode . scheme-mode)
                         (mode . geiser-repl-mode)
                         (mode . geiser-doc-mode)
                         (mode . geiser-debug-mode)))
           ("Clojure" (or (mode . clojure-mode)
                          (mode . cider-repl-mode)
                          (name . "^\\*nrepl-server .*$")
                          (name . "^\\*cider-.*$")))
           ("Emacs Lisp" (or (mode . emacs-lisp-mode)
                             (mode . inferior-emacs-lisp-mode)))
           ("Email" (mode . message-mode))
           ("EWW" (or (mode . eww-buffers-mode)
                      (mode . eww-mode)
                      (mode . eww-history-mode)
                      (mode . eww-bookmark-mode)
                      (name . "^\\*eww[ -].*$")))
           ("Image" (mode . image-mode))
           ("VCS" (or (name . "^\\*vc\\*$")
                      (name . "^\\*vc-.*$")
                      (name . "^\\*log-edit-files\\*$")))
           ("Tramp" (name . "^\\*tramp.*$"))
           ("System" (or (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$"))))))
  (ibuffer-switch-to-saved-filter-groups "default")
  (local-unset-key (kbd "M-o")))


(add-hook 'ibuffer-mode-hook 'ibuffer-custom-setup)


;; =====
;; tramp
;; =====


(defun ido-remove-tramp-from-cache ()
  "Remove any TRAMP entries from ido cache"
  (interactive)
  (let ((remote-uri-pattern
         "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\|plinkx\\|pscp\\):"))
    (setq ido-dir-file-cache
          (cl-remove-if
           (lambda (x)
             (string-match remote-uri-pattern (car x)))
           ido-dir-file-cache))
    (setq ido-work-directory-list
          (cl-remove-if
           (lambda (s)
             (string-match remote-uri-pattern s))
           ido-work-directory-list))))


;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved


(defun ido-kill-emacs-hook ()
  (ido-remove-tramp-from-cache)
  (ido-save-history))


;; ========
;; org-mode
;; ========


(progn (require 'ox-textile)
       (require 'ox-md)
       (require 'org-tempo))


(setq org-startup-truncated nil)


(setq org-capture-templates
      `(("w" "Work" entry
         (file+headline "work.org" "inbox")
         "* TODO %?\n")
        ("f" "Fun" entry
         (file+headline "fun.org" "inbox")
         "* TODO %?\n")
        ("a" "Family" entry
         (file+headline "family.org" "inbox")
         "* TODO %?\n")
        ("e" "Education" entry
         (file+headline "education.org" "inbox")
         "* TODO %?\n")
        ("t" "Tools" entry
         (file+headline "tools.org" "inbox")
         "* TODO %?\n")))


(setq org-refile-allow-creating-parent-nodes 'confirm)


(setq org-export-with-section-numbers 0
      org-export-preserve-breaks t
      org-export-with-toc nil
      org-export-with-sub-superscripts nil)


(defun org-insert-checklist-status ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace
     (line-beginning-position)
     (line-end-position))
    (goto-char (line-end-position))
    (insert (if current-prefix-arg
                " [%]"
              " [/]"))
    (org-ctrl-c-ctrl-c)))


(define-key org-mode-map
  (kbd "C-c C-/")
  'org-insert-checklist-status)


;; babel


(setq org-confirm-babel-evaluate nil)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))


;; =============
;; hippie-expand
;; =============


(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev-visible
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-whole-kill))


(defun hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))


(defun ido-hippie-expand-with (hippie-expand-function)
  "Offer ido-based completion using the specified hippie-expand function."
  (let* ((options (hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (ido-completing-read "Completions: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found"))))


(defun hippie-expand-all ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (ido-hippie-expand-with 'hippie-expand))


;; ===
;; ido
;; ===


(ido-mode t)


(ido-everywhere t)


(setq ido-enable-flex-matching t)


(setq ido-auto-merge-work-directories-length -1)


(setq ido-use-filename-at-point 'guess)


(setq ido-use-url-at-point t)


(defun ido-open-in-external-app ()
  (interactive)
  (let ((fname (expand-file-name (ido-name (car ido-matches))
                                 ido-current-directory)))
    (message "Open in external app: %s" fname)
    (open-in-external-app fname)
    (minibuffer-keyboard-quit)))


(defun ido-find-dired ()
  (interactive)
  (run-with-timer
   0.3 nil
   `(lambda ()
      (find-dired
       ,ido-current-directory
       (read-string "Run find (with args): " find-args
                    '(find-args-history . 1)))))
  (minibuffer-keyboard-quit))


(require 'grep)


(defun ido-rgrep ()
  (interactive)
  (run-with-timer
   0.3 nil
   `(lambda ()
      (grep-compute-defaults)
      (let* ((regexp (grep-read-regexp))
             (files (grep-read-files regexp))
             (dir ,ido-current-directory))
        (rgrep regexp files dir nil))))
  (minibuffer-keyboard-quit))


(defun ido-load-file ()
  (interactive)
  (let ((fname (expand-file-name (ido-name (car ido-matches))
                                 ido-current-directory)))
    (run-with-timer
     0.3 nil `(lambda () (load-file ,fname)))
    (minibuffer-keyboard-quit)))


(defun ido-jump-to-completions ()
  (select-window (get-buffer-window ido-completion-buffer)))


(advice-add 'ido-complete :after #'ido-jump-to-completions)


(with-eval-after-load 'ido
  (define-key ido-file-dir-completion-map
    (kbd "C-c C-o")
    'ido-open-in-external-app)
  (define-key ido-file-dir-completion-map
    (kbd "M-r")
    'ido-find-dired)
  (define-key ido-file-dir-completion-map
    (kbd "M-g")
    'ido-rgrep)
  (define-key ido-file-dir-completion-map
    (kbd "C-c C-l")
    'ido-load-file))


;; ===========
;; comint-mode
;; ===========


;; use vertical tab (b) as separator in history file
;; to enable correct saving of multiline commands


(setq comint-input-ring-separator "

")


;; persistent history


(setq comint-input-ring-size 1500)


(defun comint-setup-persistent-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (let ((histfile-id (cond ((and (equal major-mode 'shell-mode)
                                     (string-match "powershell" (car (process-command process))))
                                "powershell")
                               ((equal major-mode 'sql-interactive-mode) "sql")
                               (t (replace-regexp-in-string
                                   "<.*>\\|[^a-zA-Z]" ""
                                   (process-name process))))))
        (setq-local comint-input-ring-file-name
                    (expand-file-name (format ".%s-history" histfile-id)
                                      user-emacs-directory))
        (add-hook 'kill-buffer-hook 'comint-save-history nil t)
        (comint-read-input-ring t)))))


(defun comint-save-history ()
  "Save command history to `comint-input-ring-file-name'
   (preserving existing history from that file).
   * Features
     - Preserving existing history
       i.e. in-memory history merged with history from file
     - Removing duplicated commands
   * Bugs
     - When comint-input-ring is overflowed *and* (car comint-input-ring) is zero,
       all its contents treated as 'old' history
       (and therefore goes to the tail of history file)"
  (let* ((ir (cl-coerce (cl-coerce (cddr comint-input-ring) 'list) 'vector))
         (ir-items-count (cadr comint-input-ring))
         (ir-insertion-place (car comint-input-ring))
         (ir-full-p (= ir-items-count comint-input-ring-size))
         (history-new-local-bound (cond ((not ir-full-p) (cl-position nil ir))
                                        ((zerop ir-insertion-place) 0)
                                        (t ir-insertion-place)))
         (history-new-local (cl-subseq ir 0 history-new-local-bound))
         (history-old-local (cl-subseq ir (if ir-full-p
                                              history-new-local-bound
                                            (1+ (cl-position nil ir :from-end t)))))
         (history-current-global (progn (comint-read-input-ring)
                                        (remove nil
                                                (cl-coerce (cl-coerce (cddr comint-input-ring)
                                                                      'list)
                                                           'vector))))
         (history-merged (cl-remove-duplicates
                          (cl-concatenate 'vector
                                          history-old-local
                                          history-current-global
                                          history-new-local)
                          :test #'equal))
         (history-final (cl-subseq history-merged
                                   (max 0 (- (length history-merged)
                                             comint-input-ring-size))))
         (comint-input-ring (cons 0 (cons (length history-final)
                                          history-final))))
    (comint-write-input-ring))
  (comint-read-input-ring))


(defun comint-save-history-all ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (and comint-input-ring-file-name
           (comint-save-history)))))


(add-hook 'comint-mode-hook
          'comint-setup-persistent-history)


(add-hook 'kill-emacs-hook
          'comint-save-history-all)


;; no scrolling to bottom when submitting commands


(setq-default comint-scroll-show-maximum-output nil)


;; limit output size


(add-to-list 'comint-output-filter-functions
             'comint-truncate-buffer)


;; implement ^C as simple comint-send-string


(defun comint-send-c-c ()
  (interactive)
  (comint-send-string nil ""))


(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-c C-c") 'comint-send-c-c))


;; browsing comint-input-ring


(defun comint-current-input ()
  (let ((pmax (point-max)))
    (save-excursion
      (goto-char pmax)
      (goto-char (search-backward-regexp "^"))
      (let ((prompt-position (search-forward-regexp
                              comint-prompt-regexp nil t)))
        (when (and prompt-position
                   (> pmax prompt-position))
          (buffer-substring prompt-position pmax))))))


(defun comint-query-input-ring (query)
  "Display `comint-input-ring' contents, optionally filtering it by text in command prompt"
  ;; most of code carved from comint.el, comint-dynamic-list-input-ring
  (let ((query (if (string-empty-p query) nil query))
        (history nil)
        (history-buffer " *Input History*")
        (conf (current-window-configuration)))
    (when (and (ring-p comint-input-ring)
               (not (ring-empty-p comint-input-ring)))
      (dotimes (index (ring-length comint-input-ring))
        (let ((item (ring-ref comint-input-ring index)))
          (when (or (not query) (string-match query item))
            (push item history)))))
    (when history
      (setq history (nreverse history))
      (with-output-to-temp-buffer history-buffer
        (display-completion-list history)
        (set-buffer history-buffer)
        (let ((keymap (make-sparse-keymap)))
          (set-keymap-parent keymap (current-local-map))
          (define-key keymap "\C-m"
            `(lambda ()
               (interactive)
               ;; kill current comint command line, if any
               (with-current-buffer ,completion-reference-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (beginning-of-line)
                   (when (> (point-max) (point))
                     (kill-line))))
               ;; paste selected command from history
               (comint-dynamic-list-input-ring-select)))
          (use-local-map keymap))
        (forward-line 3)
        (while (search-backward "completion" nil 'move)
          (replace-match "history reference"))
        ;; highlight query matches
        (when query
          (highlight-regexp query 'completions-common-part)))
      (sit-for 0)
      (message "Hit space to flush")
      (setq comint-dynamic-list-input-ring-window-conf conf)
      (let ((ch (read-event)))
        (if (eq ch ?\s)
            (set-window-configuration conf)
          (push ch unread-command-events))))
    (unless history (message "No history"))))


;;;; view all matching commands from within isearch


(defun modify-comint-isearch-keymap ()
  (let ((newmap (make-sparse-keymap)))
    (set-keymap-parent newmap isearch-mode-map)
    (define-key newmap (kbd "M-r")
      (lambda () (interactive)
        (comint-query-input-ring isearch-string)))
    (setq-local isearch-mode-map newmap)))


(add-hook 'comint-mode-hook
          'modify-comint-isearch-keymap)


;; ==========
;; SQL client
;; ==========


(require 'sql)


(setq sql-connection-alist
      '((example (sql-product 'sqlite))))


(add-hook 'sql-login-hook 'orgtbl-mode)


;; initial setup


(defun sql-perform-initial-commands ()
  (let ((commands (sql-get-product-feature sql-product :init-commands)))
    (when commands
      (let ((process (get-buffer-process (current-buffer))))
        (dolist (command commands)
          (comint-send-string process command)
          (comint-send-input))))))


(add-hook 'sql-login-hook 'sql-perform-initial-commands)


;; enable output accumulation


(defun sql-reset-last-command ()
  (setq-local comint-last-command (cons -1 "")))


(add-hook 'sql-login-hook
          'sql-reset-last-command)


;; reconnect


(defun sql-reconnect ()
  "Restart sql interpreter with same parameters"
  (interactive)
  (let* ((process (get-buffer-process (current-buffer)))
         (pcommand (process-command process))
         (pname (process-name process))
         (rpt (sql-make-progress-reporter nil "Login")))
    (comint-save-history) ;; save current command history
    (setq-local comint-preoutput-filter-functions
                (default-value 'comint-preoutput-filter-functions)) ;; force reset comint-preoutput-filter-functions
    (process-send-eof) ;; shutdown sql interpreter
    (sit-for 2) ;; pause for a while (ugly hack)
    (let ((pattern "
Process .+

"))
      (replace-regexp pattern "-- reconnected...\n" nil nil nil t)) ;; replace 'process finished' message with nice-looking comment
    (when (boundp 'sql-db-copies)
      (let ((sql-database-original (car sql-db-copies))
            (sql-database-copy (cadr sql-db-copies)))
        (when (and (file-exists-p sql-database-copy)
                   (not (equal (file-attribute-modification-time (file-attributes sql-database-original))
                               (file-attribute-modification-time (file-attributes sql-database-copy)))))
          (let ((c (read-key (format "What to do with temp file \"%s\"?\n[P]ush to remote host\n[any other key] - Overwrite with file from remote host, reset any local changes"
                                     sql-database-copy))))
            (cond ((char-equal c ?p)
                   (copy-file sql-database-copy
                              sql-database-original
                              t))
                  ((file-exists-p sql-database-original)
                   (copy-file sql-database-original
                              sql-database-copy
                              t t))
                  (t (delete-file sql-database-copy))))))) ;; take remote db into account. See `sql-handle-remote-db'
    (apply #'make-comint-in-buffer
           pname (current-buffer) (car pcommand) nil (cdr pcommand)) ;; start fresh instance of sql interpreter
    (let ((sql-interactive-product sql-product))
      (sql-interactive-mode)) ;; turn on sql-interactive-mode
    (progn (let ((proc (get-buffer-process (current-buffer)))
                 (secs sql-login-delay)
                 (step 0.3))
             (while (and proc
                         (memq (process-status proc) '(open run))
                         (or (accept-process-output proc step)
                             (<= 0.0 (setq secs (- secs step))))
                         (progn (goto-char (point-max))
                                (not (re-search-backward sql-prompt-regexp 0 t))))
               (sql-progress-reporter-update rpt)))
           (goto-char (point-max))
           (run-hooks 'sql-login-hook)
           (sql-progress-reporter-done rpt)
           (goto-char (point-max))) ;; tracking interpreter startup process. Stolen from `sql-product-interactive'
    ))


(define-key sql-interactive-mode-map (kbd "C-c C-k") 'sql-reconnect)


;; dealing with remote dbs


(defun sql-handle-remote-db (f product params &rest args)
  (let* ((remote-p (and (not (file-remote-p default-directory))
                        (string-match "^/ssh" sql-database)))
         (sql-database-original (replace-regexp-in-string "^/ssh" "/scp"
                                                          sql-database))
         (sql-database-copy (expand-file-name (make-temp-name "db")
                                              temporary-file-directory))
         (params (if remote-p
                     (cl-substitute sql-database-copy
                                    sql-database
                                    params
                                    :test #'equal)
                   params)))
    (when (and remote-p (file-exists-p sql-database-original))
      (copy-file sql-database-original
                 sql-database-copy
                 t t))
    (let ((buffer (apply f product (cons params args))))
      (when remote-p
        (with-current-buffer buffer
          (put 'sql-db-copies 'permanent-local t)
          (setq-local sql-db-copies (list sql-database-original
                                          sql-database-copy))
          (add-hook 'kill-buffer-hook
                    `(lambda ()
                       (ring-insert comint-input-ring "--remote db cleanup")
                       (when (get-buffer-process (current-buffer))
                         (process-send-eof))
                       (when (and (file-exists-p ,sql-database-copy)
                                  (not (equal (file-attribute-modification-time (file-attributes ,sql-database-original))
                                              (file-attribute-modification-time (file-attributes ,sql-database-copy)))))
                         (let ((c (read-key (format "What to do with temp file \"%s\"?\n[P]ush to remote host\n[S]ave as...\n[any other key] - delete"
                                                    ,sql-database-copy))))
                           (cond ((char-equal c ?p)
                                  (copy-file ,sql-database-copy
                                             ,sql-database-original
                                             t))
                                 ((char-equal c ?s)
                                  (copy-file ,sql-database-copy
                                             (read-file-name "Save file as: ")
                                             t)))))
                       (delete-file ,sql-database-copy))
                    nil t))))))


(advice-add 'sql-comint :around 'sql-handle-remote-db)


;; pretty-printing result sets using org-mode


(defun make-sql-table-pprint-filter (table-parser)
  (let ((prettify `(lambda (text)
                     (let ((table (funcall ',table-parser text)))
                       (when table
                         (orgtbl-to-orgtbl (append (cons 'hline
                                                         (cons (car table)
                                                               (cons 'hline (cdr table))))
                                                   '(hline))
                                           nil))))))
    `(lambda (string)
       (if (and string
                (not (string-match "\\*\\*\\* .* \\*\\*\\*" string))
                (string-match "^select .*;$" (ring-ref comint-input-ring 0))) ;; if last command was 'select'...
           (let ((current-command-index (cadr comint-input-ring)))
             (unless (= current-command-index (car comint-last-command))
               (setq-local comint-last-command (cons current-command-index ""))) ;; reset output accumulator if needed
             (setq-local comint-last-command
                         (cons current-command-index
                               (concat (cdr comint-last-command)
                                       string))) ;; accumulate another output chunk
             (when (string-match comint-prompt-regexp string) ;; when we have prompt string in another output chunk, ...
               (let* ((payload-raw (cdr comint-last-command)) ;; get all accumulated output...
                      (prompt-index (string-match comint-prompt-regexp payload-raw))
                      (prompt (substring payload-raw prompt-index))
                      (payload (string-trim (replace-regexp-in-string
                                             comint-prompt-regexp
                                             ""
                                             payload-raw)))) ;; then cut prompt from payload
                 (sql-reset-last-command)
                 (format "%s%s"
                         (if (string-to-list payload) ;; if payload is non-empty...
                             (format "%s\n" (or (funcall ,prettify payload) payload)) ;; then try to convert it into pretty table
                           "")
                         prompt))))
         string)))) ;; else return input unchanged


(defun sql-setup-pprint-tables ()
  (let ((table-parser (sql-get-product-feature sql-product :table-parser)))
    (sql-reset-last-command) ;; setup output accumulator
    (when table-parser
      (ring-insert comint-input-ring "--tables pprint enabled") ;; hack for preventing influence of previous history on startup
      (setq-local comint-preoutput-filter-functions
                  (cons (car comint-preoutput-filter-functions)
                        (cons (make-sql-table-pprint-filter table-parser)
                              (cdr comint-preoutput-filter-functions)))))))


(add-hook 'sql-login-hook
          'sql-setup-pprint-tables)


;;;; provide alternative table view (for tables which is too wide)


(defun convert-table-to-record-list ()
  (interactive)
  (when (org-table-p)
    (let* ((table (remove 'hline (org-table-to-lisp)))
           (header (car table))
           (rows (cdr table))
           (record-list (cons 'hline
                              (apply #'append (mapcar (lambda (row) (append (cl-mapcar #'list header row) '(hline)))
                                                      rows)))))
      (delete-region (org-table-begin) (org-table-end))
      (insert (format "%s\n" (orgtbl-to-orgtbl record-list nil)))
      (goto-char (1- (org-table-end))))))


(define-key sql-interactive-mode-map (kbd "C-c C-p") 'convert-table-to-record-list)


;; interbase


(sql-set-product-feature 'interbase :init-commands '("set list on;"))


(defun parse-isql-table (text)
  (unless (string-match "Dynamic SQL Error" text)
    (let* ((records-raw (split-string text "\n\n" t))
           (records (mapcar (lambda (r) (mapcar (lambda (x) (list (replace-regexp-in-string " .+$" "" x)
                                                                  (string-trim (replace-regexp-in-string "^[^ ]+ +" "" x))))
                                                (split-string r "\n")))
                            records-raw))
           (header (mapcar #'car (car records)))
           (rows (mapcar (lambda (r) (mapcar #'cadr r)) records)))
      (cons header rows))))


(sql-set-product-feature 'interbase :table-parser 'parse-isql-table)


;; sqlite


(add-to-list 'sql-sqlite-options "-interactive")


(sql-set-product-feature 'sqlite :init-commands '(".headers on"))


(defun parse-sqlite-table (text)
  (unless (string-match "^Error: " text)
    (mapcar (lambda (r) (split-string r "|"))
            (split-string text "\n" t))))


(sql-set-product-feature 'sqlite :table-parser 'parse-sqlite-table)


;; ===
;; xml
;; ===


;; use sgml mode for xml files


(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "xslt" "xsl" "html" "htm" "wsdl" "xml.template" "xhtml" "jsp" "pom" "jmx") t) "\\'") 'sgml-mode))


(setq sgml-basic-offset 4)


(define-abbrev-table 'sgml-mode-abbrev-table
  '(("html" "<!DOCTYPE html>
<html>
<head>
    <title>html</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
</head>
<body>


</body>
</html>")
    ("a" "<a href=\"?\">?</a>")
    ("img" "<img src=\"?\"/>")
    ("table" "<table>
<tr><th>?</th><th>?</th></tr>
<tr><td>?</td><td>?</td></tr>
</table>")
    ("tablecss" "table {
               border: 1px solid #ccc;
               border-collapse:collapse;
               font-family: 'Segoe UI', sans-serif;
               font-size: 80%;
               width:100%;
       }

       table td, table th {
               padding: 1ex;
               border: 1px solid #ccc;
               <!-- text-align:center; -->
       }")))


(defun wrap-with-tags (tag)
  "Wrap current word (or region) with given xml tag"
  (interactive "sTag: ")
  (wrap-with-text (format "<%s>" tag)
                  (format "</%s>" tag)))


;; =======
;; cc-mode
;; =======


(setq-default c-basic-offset 4)


(add-hook 'c-mode-hook
          (lambda () (c-set-style "k&r")))


(define-abbrev-table 'c-mode-abbrev-table
  '(("main" "#include <stdio.h>\n\n\nint main(int argc, char **argv)
{
     printf(\"%s\\n\", \"hello world\");
     return 0;
}")
    ("mainv" "#include <stdio.h>\n\n\nint main(void)
{
     printf(\"%s\\n\", \"hello world\");
     return 0;
}")
    ("pr" "printf(\"%d\\n\", 42);")
    ("fori" "for (int i = 0; i < 42; i++)\n{\n\n}")
    ("whl" "while ()\n{\n\n}")
    ("whlc" "int c;\nwhile ((c = getchar()) != '\\n')\n{\n\n}")
    ("iff" "if (i == 42)\n{\n\n}")
    ("ife" "if (i == 42)\n{\n\n} else {\n\n}")
    ("eif" "else if (i == 99) {\n\n}")
    ("fun" "void f()\n{\n\n}")))


;; ====
;; java
;; ====


(add-hook 'java-mode-hook
          (lambda () (c-set-style "user")))


(define-abbrev-table 'java-mode-abbrev-table
  '(("psvm" "public static void main(String[] args) {


}")
    ("sout" "System.out.println(\"\");")
    ("fori" "for (int i = 0; i < 42; i++) {\n\n}")))


;; =======
;; clojure
;; =======


(setq cider-repl-history-file
      (expand-file-name ".cider-history" user-emacs-directory))


(setq cider-show-error-buffer nil)


;; ===========
;; common lisp
;; ===========


(setq inferior-lisp-program "clisp")


(add-to-list 'auto-mode-alist
             '("\\.cl\\'" . common-lisp-mode))


(setq slime-contribs '(slime-fancy))


(defun use-eww-for-cl-hyperspec-lookup ()
  (setq-local browse-url-browser-function
              'eww-browse-url))


(add-hook 'lisp-mode-hook 'use-eww-for-cl-hyperspec-lookup)


(add-hook 'slime-repl-mode-hook 'use-eww-for-cl-hyperspec-lookup)


;; ======
;; Python
;; ======


(require 'python)


(defun python-make-venv (path)
  (interactive "Gvenv directory: ")
  (shell-command
   (format "python -m venv --system-site-packages '%s'" path)))


(defun python-set-venv (path-to-venv)
  (interactive "fPath to venv: ")
  (setq python-shell-virtualenv-root path-to-venv)
  (message "Set python venv to %s" path-to-venv))


(defun python-reset-venv ()
  (interactive)
  (setq python-shell-virtualenv-root nil)
  (message "Reset python venv"))


(defun run-python-with-venv ()
  "Invoke run-python command, optionally setup venv, if current
   directory has 'venv' folder.  To manually setup venv, use
   `python-set-venv'/`python-reset-venv' commands"
  (interactive)
  (let ((venv (file-truename "venv")))
    (if (file-exists-p venv)
        (let ((python-shell-virtualenv-path venv))
          (run-python)
          (message "Using venv: %s"))
      (run-python))))


(define-key python-mode-map (kbd "C-c C-p") 'run-python-with-venv)


(define-abbrev-table 'python-mode-abbrev-table
  '(("main" "if __name__ == '__main__':\n")
    ("fori" "for x in xs:\n")))


;; =======
;; isearch
;; =======


(defun isearch-select-search-string ()
  (interactive)
  (isearch-done)
  (set-mark (point))
  (if isearch-forward
      (funcall (if isearch-regexp
                   #'search-backward-regexp
                 #'search-backward)
               isearch-string)
    (funcall (if isearch-regexp
                 #'search-forward-regexp
               #'search-forward)
             isearch-string)))


(defun isearch-append-wildcard ()
  "Append .* to current isearch query"
  (interactive)
  (with-isearch-suspended
   (setq isearch-new-string (concat isearch-string ".*")
         isearch-new-message isearch-new-string)))


(progn
  (define-key isearch-mode-map (kbd "M-q") 'isearch-query-replace)
  (define-key isearch-mode-map (kbd "M-.") 'isearch-append-wildcard)
  (define-key isearch-mode-map (kbd "C-SPC") 'isearch-select-search-string))


;; ===========
;; ssh tunnels
;; ===========


(setq ssh-tunnels nil)


(defun ssh-tunnel (&rest args)
  "Start ssh tunnel by with a command 'ssh -NvL ...' in a buffer with descriptive name.
   By default, predefined `ssh-tunnels' list is used.
   Example config:
   (setq ssh-tunnels
         '((\"my-tunnel\" \"localhost:4444:localhost:8888" "hostname\")
           (\"my-tunnel-2\" \"localhost:4445:localhost:8888" "user@host2\"))
  With universal argument, asks for arbitrary tunnel parameters"
  (interactive)
  (let* ((forwarding-options
          (or args
              (if current-prefix-arg
                  (list (read-string "Create ssh tunnel: ")
                        (format "%s:%s"
                                (read-string "Local socket: ")
                                (read-string "Remote socket: " "localhost:"))
                        (read-string "Ssh connection spec (e.g. user@host): "))
                (assoc (ido-completing-read
                        "Create ssh tunnel: "
                        (mapcar #'car ssh-tunnels))
                       ssh-tunnels))))
         (tunnel-name (car forwarding-options))
         (sockets (cadr forwarding-options))
         (ssh-spec (caddr forwarding-options))
         (default-directory "~")
         (buffer (format "*ssh-tunnel/%s*" tunnel-name))
         (args (list "shell" buffer "ssh" "-vNL" sockets ssh-spec)))
    (message "%s" (string-join (cddr args) " "))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (apply #'start-process args)
    (with-current-buffer buffer
      (shell-mode))
    (set-process-filter (get-buffer-process buffer)
                        #'comint-output-filter)))


;; =====
;; shell
;; =====


(require 'shell)


(defun run-shell (preset-name)
  "M-x shell on steroids.
   Start local or remote shell using set of presets (See `shell-presets' variable).
   Each preset is a pair of (<\"preset-name\"> . <options-alist>)
   Legal values in options-alist are:
   |-------------------+------------------------------------------------|
   | option            | description                                    |
   |-------------------+------------------------------------------------|
   | file-name         | Path to shell executable                       |
   | working-directory | Working directory for shell instance           |
   |                   | Useful for defining remote shell sessions      |
   | startup-fn        | Function to call for starting the shell        |
   |                   | By default, function `shell' is used           |
   | codings           | Explicit decoding and encoding systems         |
   |                   | (List of two symbols, e.g. '(cp1251-dos utf-8) |
   |-------------------+------------------------------------------------|"
  (interactive (list (ido-completing-read
                      "Shell: "
                      (mapcar #'car shell-presets))))
  (let* ((shell-options (cdr (assoc preset-name shell-presets)))
         (startup-fn (alist-get 'startup-fn shell-options))
         (codings (alist-get 'codings shell-options))
         (buffer-name (format "*%s*" preset-name))
         (dead-buffer-exists (and (get-buffer buffer-name)
                                  (not (get-buffer-process buffer-name))))
         (wd (or (when dead-buffer-exists
                   (with-current-buffer buffer-name
                     (when (file-exists-p default-directory)
                       default-directory)))
                 (alist-get 'working-directory
                            shell-options)
                 (ido-read-directory-name "wd: "))))
    (if dead-buffer-exists
        (with-current-buffer buffer-name
          (comint-save-history))
      (setq buffer-name (generate-new-buffer-name buffer-name)))
    (let ((w (cl-find-if (lambda (x) (equal (window-buffer x)
                                            (get-buffer buffer-name)))
                         (window-list))))
      (when w (select-window w)))
    (switch-to-buffer buffer-name)
    (cd wd)
    (if startup-fn
        (funcall startup-fn buffer-name)
      (let ((explicit-shell-file-name (alist-get 'file-name shell-options)))
        (shell buffer-name)))
    (when codings
      (set-buffer-process-coding-system (car codings) (cadr codings)))))


(setq shell-presets
      ;; Default preset, same as M-x shell
      '(("shell")))


;; ================================
;; Access eng-rus dictionary online
;; ================================


(defun translate-en-ru-online (&optional query)
  "Translate from english to russian or vice versa (depending on query)"
  (interactive)
  (let* ((default-directory "~")
         (query (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (or query (read-string "Translate: "))))
         (en-ru `((command . ,(concat "bash -c \"curl -sL '%s"
                                      "' | sed -rn '/span class=.trans/ {s:.*<span.*>(.*[^ ]) *<.span>.*:\\1:g ; p}'"
                                      " | sort | uniq\""))
                  (link . ,(format "https://dictionary.cambridge.org/search/direct/?datasetsearch=english-russian&q=%s"
                                   (url-encode-url query)))))
         (ru-en `((command . ,(concat "bash -c \"curl -sL '%s"
                                      "' | grep -oP '(?<=class=.tl.>)[^<]+' | head -n 3\""))
                  (link . ,(format "https://en.openrussian.org/ru/%s"
                                   (url-encode-url query)))))
         (preset (if (string-match "[a-zA-Z]" query)
                     en-ru ru-en))
         (link (cdr (assoc 'link preset)))
         (command (format (cdr (assoc 'command preset)) link))
         (translation (shell-command-to-string command)))
    (message "%s" command)
    (if (zerop (length translation))
        (message "Can't find translation for '%s'" query)
      (message "%s =>\n%s\n%s" query translation link))))


;; ================
;; compilation-mode
;; ================


(require 'ansi-color)


(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))


(add-hook 'compilation-filter-hook
          #'colorize-compilation)


(setq compilation-scroll-output t)


;; ===========
;; abbrev-mode
;; ===========


;; disable prompt about saving abbrevs


(setq save-abbrevs nil)


;; ===========================
;; load site-specific settings
;; ===========================


(let ((site-file (expand-file-name "site.el" user-emacs-directory)))
  (when (file-exists-p site-file)
    (load-file site-file)))
