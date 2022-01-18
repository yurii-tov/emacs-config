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
  (global-set-key (kbd "C-c j") 'repls-map)
  (define-key 'repls-map (kbd "j") 'run-shell)
  (define-key 'repls-map (kbd "i") 'ielm)
  (define-key 'repls-map (kbd "l") 'slime)
  (define-key 'repls-map (kbd "p") 'run-python-with-venv)
  (define-key 'repls-map (kbd "q") 'sql-connect)
  (define-key 'repls-map (kbd "k") 'cider-connect)
  (define-key 'repls-map (kbd "K") 'cider-jack-in))


;; extending global search map


(progn
  (define-key search-map (kbd "f") 'find-dired)
  (define-key search-map (kbd "m") 'count-matches)
  (define-key search-map (kbd "g") 'rgrep)
  (define-key search-map (kbd "t") 'translate-en-ru-online))


;; transforming text


(progn
  (define-prefix-command 'text-transform-map)
  (global-set-key (kbd "M-c") 'text-transform-map)
  (define-key 'text-transform-map (kbd "c") 'upcase-char)
  (define-key 'text-transform-map (kbd "s") 'sort-lines)
  (define-key 'text-transform-map (kbd "x") 'shuffle-lines)
  (define-key 'text-transform-map (kbd "r") 'replace-string)
  (define-key 'text-transform-map (kbd "u") 'upcase-dwim)
  (define-key 'text-transform-map (kbd "d") 'downcase-dwim)
  (define-key 'text-transform-map (kbd "M-d") 'delete-duplicate-lines)
  (define-key 'text-transform-map (kbd "M-c") 'duplicate-line)
  (define-key 'text-transform-map (kbd "p") 'fill-paragraph)
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


;; inserting things


(progn
  (define-prefix-command 'insert-map)
  (global-set-key (kbd "C-x i") 'insert-map)
  (define-key 'insert-map (kbd "f") 'insert-file)
  (define-key 'insert-map (kbd "n") 'insert-buffer-name)
  (define-key 'insert-map (kbd "b") 'insert-buffer)
  (define-key 'insert-map (kbd "c") 'insert-char)
  (define-key 'insert-map (kbd "i") 'insert-from-kill-ring)
  (define-key 'insert-map (kbd "a") 'insert-unicode-arrow)
  (define-key 'insert-map (kbd "m") 'insert-unicode-math)
  (define-key 'insert-map (kbd "e") 'insert-unicode-emoji))


;; evaluating emacs lisp


(progn
  (define-prefix-command 'eval-elisp-map)
  (global-set-key (kbd "C-c e") 'eval-elisp-map)
  (define-key 'eval-elisp-map (kbd "e") 'eval-last-sexp)
  (define-key 'eval-elisp-map (kbd "f") 'load-file)
  (define-key 'eval-elisp-map (kbd "r") 'elisp-eval-region-or-buffer))


;; misc


(let ((bindings
       '(("M-x" smex)
         ("M-=" count-words)
         ("C-x C-b" ibuffer)
         ("C-x l" hl-line-mode)
         ("C-x C-l" transpose-lines)
         ("M-i" async-shell-command)
         ("C-c n" make-scratch-buffer)
         ("C-c z" zone)
         ("C-c p" copy-file-name-to-clipboard)
         ("M-k" kill-whole-line)
         ("M-q" hippie-expand)
         ("C-v" scroll-up-5-lines)
         ("M-v" scroll-down-5-lines)
         ("M-l" reindent-region)
         ("M-u" force-revert-buffer)
         ("C-c r" rename-buffer)
         ("C-c h" hexl-mode)
         ("C-c a" org-agenda)
         ("C-c c" org-capture)
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


(require 'hi-lock)


(defun count-lwc ()
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start)))
    (propertize (format "[lines:%d words:%d chars:%d]" lines words chars)
                'face 'hi-green)))


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


;; slower scrolling


(defun scroll-down-5-lines ()
  (interactive)
  (scroll-down-command 5))


(defun scroll-up-5-lines ()
  (interactive)
  (scroll-up-command 5))


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


(setq ls-lisp-use-insert-directory-program nil
      ls-lisp-format-time-list
      '("%Y-%m-%d %H:%M"
        "%Y-%m-%d %H:%M")
      ls-lisp-use-localized-time-format t
      ls-lisp-dirs-first t)


(setq dired-listing-switches "-alh"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun dired-open-in-external-app ()
  (interactive)
  (mapc #'open-in-external-app (dired-get-marked-files)))


(with-eval-after-load 'dired
  (define-key dired-mode-map
    (kbd "M-s f f")
    'find-dired)
  (define-key dired-mode-map
    (kbd "C-c C-o")
    'dired-open-in-external-app)
  (define-key dired-mode-map
    (kbd "`")
    'dired-hide-details-mode))


(add-hook 'dired-mode-hook 'dired-hide-details-mode)


(defun dired-custom-highlight ()
  (highlight-regexp " [0-9]+\\-[0-9][0-9]\\-[0-9][0-9] [0-9][0-9]:[0-9][0-9] "
                    'font-lock-string-face))


(add-hook 'dired-hide-details-mode-hook 'dired-custom-highlight)


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


(defun shuffle-lines ()
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (list (region-beginning)
                           (region-end))
                   (list (point-min)
                         (point-max))))
         (lines (split-string (apply #'buffer-substring bounds)
                              "\n" t " *"))
         (lines-count (length lines))
         (indexes (cl-loop for i from 0 below lines-count collect i))
         (shuffled (make-vector lines-count nil)))
    (dolist (x lines)
      (let ((i (nth (random lines-count) indexes)))
        (setf (aref shuffled i) x)
        (setq lines-count (1- lines-count))
        (setq indexes (cl-remove i indexes))))
    (apply #'delete-region bounds)
    (insert (string-join shuffled "\n"))))


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


(defun insert-from-kill-ring (text)
  (interactive
   (list (ido-completing-read
          "Insert from kill ring: "
          (cl-remove-duplicates kill-ring :test #'equal))))
  (insert text))


(defun make-scratch-buffer ()
  (interactive)
  (let* ((name (generate-new-buffer-name "*scratch*")))
    (switch-to-buffer name)
    (org-mode)
    (insert "#+TITLE: ?\n\n\n")))


;; hippie-expand


(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev-visible
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-whole-kill))


;; disable prompt about saving abbrevs


(setq save-abbrevs nil)


;; insert various unicode chars


(defun unicode-describe-regions (&rest regions)
  "Turns given unicode region specs into a human-readable list of definitions.
The specs are either a list denoting range, e.g. '(80 122), or an integer.
Example:
> (unicode-describe-regions 60 '(80 83) #x100)
> (\"LESS-THAN SIGN [<]\" \"LATIN CAPITAL LETTER P [P]\" \"LATIN CAPITAL LETTER Q [Q]\" \"LATIN CAPITAL LETTER R [R]\" \"LATIN CAPITAL LETTER S [S]\" \"LATIN CAPITAL LETTER A WITH MACRON [Ā]\")"
  (mapcan (lambda (r)
            (let ((r (if (listp r) r (list r))))
              (cl-loop for x from (car r) upto (or (cadr r) (car r))
                       collect
                       (concat (get-char-code-property x 'name)
                               (format " [%s]" (string x))))))
          regions))


(defun unicode-select-char (chars)
  "Prompts user with a choice from list of chars.
Expects args like `unicode-describe-regions' output"
  (let* ((char-name (replace-regexp-in-string
                     "....$" ""
                     (ido-completing-read "Char: " chars))))
    (char-from-name char-name)))


(defun insert-unicode-arrow ()
  (interactive)
  (insert
   (unicode-select-char
    (unicode-describe-regions
     #x003C #x003E #x005E #x00AB #x00BB #x02C2 #x02C3 #x02C4 #x02C5 #x02C6
     #x02C7 #x02EC #x02F0 #x02F1 #x02F2 #x0311 #x032C #x032D #x1401 #x1403
     #x1405 #x1409 #x140A #x141E #x142F #x1431 #x1433 #x1438 #x1449 #x15D0
     #x15D1 #x15D2 #x15D5 #x16E3 #x1F501 #x1F502 #x1F503 #x1F504 #x1F5D8
     #x1F791 #x1F792 #x1F793 #x1F810 #x1F811 #x1F812 #x1F813 #x1F814
     #x1F815 #x1F816 #x1F817 #x1F838 #x1F839 #x1F83A #x1F83B #x1F83C
     #x1F83D #x1F83E #x1F83F #x1F840 #x1F841 #x1F842 #x1F843 #x1F844
     #x1F845 #x1F846 #x1F846 #x1F847 #x1F860 #x1F861 #x1F862 #x1F863
     #x1F864 #x1F865 #x1F866 #x1F867 #x1F868 #x1F869 #x1F86A #x1F86B
     #x1F86C #x1F86D #x1F86E #x1F86F #x1F870 #x1F871 #x1F872 #x1F873
     #x1F874 #x1F875 #x1F876 #x1F877 #x1F878 #x1F879 #x1F87A #x1F87B
     #x1F87C #x1F87D #x1F87E #x1F87F #x1F880 #x1F881 #x1F882 #x1F883
     #x1F884 #x1F885 #x1F886 #x1F887 #x1F890 #x1F891 #x1F892 #x1F893
     #x1F894 #x1F895 #x1F896 #x1F897 #x1F89C #x1F89D #x1F89E #x1F89F
     #x1F8A0 #x1F8A1 #x1F8A2 #x1F8A3 #x1F8A4 #x1F8A5 #x1F8A6 #x1F8A7
     #x1F8A8 #x1F8A9 #x1F8AA #x1F8AB #x1F8AC #x1F8AD #x2039 #x203A #x20D5
     #x20D6 #x20D7 #x2190 #x2191 #x2192 #x2193 #x2194 #x2195 #x2196 #x2197
     #x2198 #x2199 #x219A #x219B #x219C #x219D #x219E #x219F #x21A0 #x21A1
     #x21A2 #x21A3 #x21A4 #x21A5 #x21A6 #x21A7 #x21A8 #x21A9 #x21AA #x21AB
     #x21AC #x21AD #x21AE #x21AF #x21B0 #x21B1 #x21B2 #x21B3 #x21B4 #x21B5
     #x21B6 #x21B7 #x21B8 #x21B9 #x21BA #x21BB #x21BC #x21BD #x21BE #x21BF
     #x21C0 #x21C1 #x21C2 #x21C3 #x21C4 #x21C5 #x21C6 #x21C7 #x21C8 #x21C9
     #x21CA #x21CB #x21CC #x21CD #x21CF #x21D0 #x21D1 #x21D2 #x21D3 #x21D4
     #x21D5 #x21D6 #x21D7 #x21D8 #x21D9 #x21DA #x21DB #x21DC #x21DD #x21DE
     #x21DF #x21E0 #x21E1 #x21E2 #x21E3 #x21E4 #x21E5 #x21E6 #x21E7 #x21E8
     #x21E9 #x21EA #x21EB #x21EC #x21ED #x21EE #x21EF #x21F0 #x21F1 #x21F2
     #x21F3 #x21F4 #x21F5 #x21F6 #x21F7 #x21F8 #x21F9 #x21FA #x21FB #x21FC
     #x21FD #x21FE #x21FF #x2303 #x2304 #x2343 #x2344 #x2345 #x2346 #x2347
     #x2348 #x234C #x234D #x234F #x2350 #x2353 #x2354 #x2356 #x2357 #x25A0
     #x25A1 #x25A4 #x25A6 #x25A7 #x25A8 #x25B2 #x25B6 #x25BC #x25C0 #x2607
     #x2608 #x261A #x261B #x261D #x261E #x261F #x276E #x276F #x2770 #x2771
     #x2794 #x2798 #x2799 #x279A #x279B #x279C #x279D #x279E #x279F #x27A0
     #x27A1 #x27A2 #x27A3 #x27A4 #x27A5 #x27A6 #x27A7 #x27A8 #x27A9 #x27AA
     #x27AB #x27AC #x27AD #x27AE #x27AF #x27B1 #x27B2 #x27B3 #x27B4 #x27B5
     #x27B6 #x27B7 #x27B8 #x27B9 #x27BA #x27BB #x27BC #x27BD #x27BE #x27F0
     #x27F1 #x27F2 #x27F3 #x27F4 #x27F5 #x27F6 #x27F7 #x27F8 #x27F9 #x27FB
     #x27FC #x27FD #x27FE #x27FF #x2900 #x2901 #x2902 #x2903 #x2904 #x2905
     #x2906 #x2907 #x2908 #x2909 #x290A #x290B #x290C #x290D #x290E #x290F
     #x2910 #x2911 #x2912 #x2913 #x2914 #x2915 #x2916 #x2917 #x2918 #x2919
     #x291A #x291B #x291C #x291D #x291E #x291F #x2920 #x2921 #x2922 #x2923
     #x2924 #x2925 #x2926 #x2927 #x2928 #x2929 #x292956 #x292A #x292D
     #x292E #x292F #x2930 #x2931 #x2932 #x2933 #x2934 #x2935 #x2936 #x2937
     #x2938 #x2939 #x293A #x293B #x293C #x293D #x293E #x293F #x2940 #x2941
     #x2942 #x2943 #x2944 #x2945 #x2946 #x2947 #x2948 #x2949 #x294A #x294B
     #x294C #x294D #x294E #x294F #x2950 #x2951 #x2952 #x2953 #x2954 #x2955
     #x2956 #x2957 #x2958 #x2959 #x295A #x295B #x295C #x295D #x295E #x295F
     #x2960 #x2961 #x2962 #x2963 #x2964 #x2965 #x2966 #x2967 #x2968 #x2969
     #x296A #x296B #x296C #x296D #x296E #x296F #x2970 #x2971 #x2972 #x2973
     #x2974 #x2976 #x2977 #x2978 #x2979 #x297A #x297C #x297D #x297E #x297F
     #x29C8 #x2B00 #x2B01 #x2B02 #x2B03 #x2B04 #x2B05 #x2B06 #x2B07 #x2B08
     #x2B09 #x2B0A #x2B0B #x2B0C #x2B0D #x2B33 #x2B34 #x2B35 #x2B36 #x2B39
     #x2B3A #x2B3B #x2B3C #x2B3D #x2B3F #x2B6E #x2B6F #x2B70 #x2B71 #x2B72
     #x2B73 #x2B76 #x2B77 #x2B78 #x2B79 #x2B7A #x2B7B #x2B7C #x2B7D #x2B7E
     #x2B7F #x2B80 #x2B81 #x2B82 #x2B83 #x2B84 #x2B85 #x2B86 #x2B87 #x2B88
     #x2B89 #x2B8A #x2B8B #x2B8C #x2B8D #x2B8E #x2B8F #x2B94 #x2B98 #x2B99
     #x2B9A #x2B9B #x2B9C #x2B9D #x2B9E #x2B9F #x2BA8 #x2BA9 #x2BAA #x2BAB
     #x2BAC #x2BAD #x2BAE #x2BAF #x2BB0 #x2BB1 #x2BB2 #x2BB3 #x2BB4 #x2BB5
     #x2BB6 #x2BB7 #x2BEC #x2BED #x2BEE #x2BEF))))


(defun insert-unicode-math ()
  (interactive)
  (insert (unicode-select-char (unicode-describe-regions '(#x2200 #x22FF)
                                                         '(#x27C0 #x27EF)
                                                         '(#x2980 #x29FF)
                                                         '(#x2A00 #x2AFF)
                                                         '(#x0370 #x03FF)
                                                         '(#x1D400 #x1D7FF)))))


(defun insert-unicode-emoji ()
  (interactive)
  (insert
   (unicode-select-char
    (unicode-describe-regions
     #X00A9 #X00AE #X1F004 #X1F0CF #X1F170 #X1F171 #X1F17E #X1F17F #X1F18E #X1F191
     #X1F192 #X1F193 #X1F194 #X1F195 #X1F196 #X1F197 #X1F198 #X1F199 #X1F19A #X1F201
     #X1F202 #X1F21A #X1F22F #X1F232 #X1F233 #X1F234 #X1F235 #X1F236 #X1F237 #X1F238
     #X1F239 #X1F23A #X1F250 #X1F251 #X1F300 #X1F301 #X1F302 #X1F303 #X1F304 #X1F305
     #X1F306 #X1F307 #X1F308 #X1F309 #X1F30A #X1F30B #X1F30C #X1F30D #X1F30E #X1F30F
     #X1F310 #X1F311 #X1F312 #X1F313 #X1F314 #X1F315 #X1F316 #X1F317 #X1F318 #X1F319
     #X1F31A #X1F31B #X1F31C #X1F31D #X1F31E #X1F31F #X1F320 #X1F321 #X1F324 #X1F325
     #X1F326 #X1F327 #X1F328 #X1F329 #X1F32A #X1F32B #X1F32C #X1F32D #X1F32E #X1F32F
     #X1F330 #X1F331 #X1F332 #X1F333 #X1F334 #X1F335 #X1F336 #X1F337 #X1F338 #X1F339
     #X1F33A #X1F33B #X1F33C #X1F33D #X1F33E #X1F33F #X1F340 #X1F341 #X1F342 #X1F343
     #X1F344 #X1F345 #X1F346 #X1F347 #X1F348 #X1F349 #X1F34A #X1F34B #X1F34C #X1F34D
     #X1F34E #X1F34F #X1F350 #X1F351 #X1F352 #X1F353 #X1F354 #X1F355 #X1F356 #X1F357
     #X1F358 #X1F359 #X1F35A #X1F35B #X1F35C #X1F35D #X1F35E #X1F35F #X1F360 #X1F361
     #X1F362 #X1F363 #X1F364 #X1F365 #X1F366 #X1F367 #X1F368 #X1F369 #X1F36A #X1F36B
     #X1F36C #X1F36D #X1F36E #X1F36F #X1F370 #X1F371 #X1F372 #X1F373 #X1F374 #X1F375
     #X1F376 #X1F377 #X1F378 #X1F379 #X1F37A #X1F37B #X1F37C #X1F37D #X1F37E #X1F37F
     #X1F380 #X1F381 #X1F382 #X1F383 #X1F384 #X1F385 #X1F386 #X1F387 #X1F388 #X1F389
     #X1F38A #X1F38B #X1F38C #X1F38D #X1F38E #X1F38F #X1F390 #X1F391 #X1F392 #X1F393
     #X1F396 #X1F397 #X1F399 #X1F39A #X1F39B #X1F39E #X1F39F #X1F3A0 #X1F3A1 #X1F3A2
     #X1F3A3 #X1F3A4 #X1F3A5 #X1F3A6 #X1F3A7 #X1F3A8 #X1F3A9 #X1F3AA #X1F3AB #X1F3AC
     #X1F3AD #X1F3AE #X1F3AF #X1F3B0 #X1F3B1 #X1F3B2 #X1F3B3 #X1F3B4 #X1F3B5 #X1F3B6
     #X1F3B7 #X1F3B8 #X1F3B9 #X1F3BA #X1F3BB #X1F3BC #X1F3BD #X1F3BE #X1F3BF #X1F3C0
     #X1F3C1 #X1F3C2 #X1F3C3 #X1F3C4 #X1F3C5 #X1F3C6 #X1F3C7 #X1F3C8 #X1F3C9 #X1F3CA
     #X1F3CB #X1F3CC #X1F3CD #X1F3CE #X1F3CF #X1F3D0 #X1F3D1 #X1F3D2 #X1F3D3 #X1F3D4
     #X1F3D5 #X1F3D6 #X1F3D7 #X1F3D8 #X1F3D9 #X1F3DA #X1F3DB #X1F3DC #X1F3DD #X1F3DE
     #X1F3DF #X1F3E0 #X1F3E1 #X1F3E2 #X1F3E3 #X1F3E4 #X1F3E5 #X1F3E6 #X1F3E7 #X1F3E8
     #X1F3E9 #X1F3EA #X1F3EB #X1F3EC #X1F3ED #X1F3EE #X1F3EF #X1F3F0 #X1F3F3 #X1F3F4
     #X1F3F5 #X1F3F7 #X1F3F8 #X1F3F9 #X1F3FA #X1F3FB #X1F3FC #X1F3FD #X1F3FE #X1F3FF
     #X1F400 #X1F401 #X1F402 #X1F403 #X1F404 #X1F405 #X1F406 #X1F407 #X1F408 #X1F409
     #X1F40A #X1F40B #X1F40C #X1F40D #X1F40E #X1F40F #X1F410 #X1F411 #X1F412 #X1F413
     #X1F414 #X1F415 #X1F416 #X1F417 #X1F418 #X1F419 #X1F41A #X1F41B #X1F41C #X1F41D
     #X1F41E #X1F41F #X1F420 #X1F421 #X1F422 #X1F423 #X1F424 #X1F425 #X1F426 #X1F427
     #X1F428 #X1F429 #X1F42A #X1F42B #X1F42C #X1F42D #X1F42E #X1F42F #X1F430 #X1F431
     #X1F432 #X1F433 #X1F434 #X1F435 #X1F436 #X1F437 #X1F438 #X1F439 #X1F43A #X1F43B
     #X1F43C #X1F43D #X1F43E #X1F43F #X1F440 #X1F441 #X1F442 #X1F443 #X1F444 #X1F445
     #X1F446 #X1F447 #X1F448 #X1F449 #X1F44A #X1F44B #X1F44C #X1F44D #X1F44E #X1F44F
     #X1F450 #X1F451 #X1F452 #X1F453 #X1F454 #X1F455 #X1F456 #X1F457 #X1F458 #X1F459
     #X1F45A #X1F45B #X1F45C #X1F45D #X1F45E #X1F45F #X1F460 #X1F461 #X1F462 #X1F463
     #X1F464 #X1F465 #X1F466 #X1F467 #X1F468 #X1F469 #X1F46A #X1F46B #X1F46C #X1F46D
     #X1F46E #X1F46F #X1F470 #X1F471 #X1F472 #X1F473 #X1F474 #X1F475 #X1F476 #X1F477
     #X1F478 #X1F479 #X1F47A #X1F47B #X1F47C #X1F47D #X1F47E #X1F47F #X1F480 #X1F481
     #X1F482 #X1F483 #X1F484 #X1F485 #X1F486 #X1F487 #X1F488 #X1F489 #X1F48A #X1F48B
     #X1F48C #X1F48D #X1F48E #X1F48F #X1F490 #X1F491 #X1F492 #X1F493 #X1F494 #X1F495
     #X1F496 #X1F497 #X1F498 #X1F499 #X1F49A #X1F49B #X1F49C #X1F49D #X1F49E #X1F49F
     #X1F4A0 #X1F4A1 #X1F4A2 #X1F4A3 #X1F4A4 #X1F4A5 #X1F4A6 #X1F4A7 #X1F4A8 #X1F4A9
     #X1F4AA #X1F4AB #X1F4AC #X1F4AD #X1F4AE #X1F4AF #X1F4B0 #X1F4B1 #X1F4B2 #X1F4B3
     #X1F4B4 #X1F4B5 #X1F4B6 #X1F4B7 #X1F4B8 #X1F4B9 #X1F4BA #X1F4BB #X1F4BC #X1F4BD
     #X1F4BE #X1F4BF #X1F4C0 #X1F4C1 #X1F4C2 #X1F4C3 #X1F4C4 #X1F4C5 #X1F4C6 #X1F4C7
     #X1F4C8 #X1F4C9 #X1F4CA #X1F4CB #X1F4CC #X1F4CD #X1F4CE #X1F4CF #X1F4D0 #X1F4D1
     #X1F4D2 #X1F4D3 #X1F4D4 #X1F4D5 #X1F4D6 #X1F4D7 #X1F4D8 #X1F4D9 #X1F4DA #X1F4DB
     #X1F4DC #X1F4DD #X1F4DE #X1F4DF #X1F4E0 #X1F4E1 #X1F4E2 #X1F4E3 #X1F4E4 #X1F4E5
     #X1F4E6 #X1F4E7 #X1F4E8 #X1F4E9 #X1F4EA #X1F4EB #X1F4EC #X1F4ED #X1F4EE #X1F4EF
     #X1F4F0 #X1F4F1 #X1F4F2 #X1F4F3 #X1F4F4 #X1F4F5 #X1F4F6 #X1F4F7 #X1F4F8 #X1F4F9
     #X1F4FA #X1F4FB #X1F4FC #X1F4FD #X1F4FF #X1F500 #X1F501 #X1F502 #X1F503 #X1F504
     #X1F505 #X1F506 #X1F507 #X1F508 #X1F509 #X1F50A #X1F50B #X1F50C #X1F50D #X1F50E
     #X1F50F #X1F510 #X1F511 #X1F512 #X1F513 #X1F514 #X1F515 #X1F516 #X1F517 #X1F518
     #X1F519 #X1F51A #X1F51B #X1F51C #X1F51D #X1F51E #X1F51F #X1F520 #X1F521 #X1F522
     #X1F523 #X1F524 #X1F525 #X1F526 #X1F527 #X1F528 #X1F529 #X1F52A #X1F52B #X1F52C
     #X1F52D #X1F52E #X1F52F #X1F530 #X1F531 #X1F532 #X1F533 #X1F534 #X1F535 #X1F536
     #X1F537 #X1F538 #X1F539 #X1F53A #X1F53B #X1F53C #X1F53D #X1F549 #X1F54A #X1F54B
     #X1F54C #X1F54D #X1F54E #X1F550 #X1F551 #X1F552 #X1F553 #X1F554 #X1F555 #X1F556
     #X1F557 #X1F558 #X1F559 #X1F55A #X1F55B #X1F55C #X1F55D #X1F55E #X1F55F #X1F560
     #X1F561 #X1F562 #X1F563 #X1F564 #X1F565 #X1F566 #X1F567 #X1F56F #X1F570 #X1F573
     #X1F574 #X1F575 #X1F576 #X1F577 #X1F578 #X1F579 #X1F57A #X1F587 #X1F58A #X1F58B
     #X1F58C #X1F58D #X1F590 #X1F595 #X1F596 #X1F5A4 #X1F5A5 #X1F5A8 #X1F5B1 #X1F5B2
     #X1F5BC #X1F5C2 #X1F5C3 #X1F5C4 #X1F5D1 #X1F5D2 #X1F5D3 #X1F5DC #X1F5DD #X1F5DE
     #X1F5E1 #X1F5E3 #X1F5E8 #X1F5EF #X1F5F3 #X1F5FA #X1F5FB #X1F5FC #X1F5FD #X1F5FE
     #X1F5FF #X1F600 #X1F601 #X1F602 #X1F603 #X1F604 #X1F605 #X1F606 #X1F607 #X1F608
     #X1F609 #X1F60A #X1F60B #X1F60C #X1F60D #X1F60E #X1F60F #X1F610 #X1F611 #X1F612
     #X1F613 #X1F614 #X1F615 #X1F616 #X1F617 #X1F618 #X1F619 #X1F61A #X1F61B #X1F61C
     #X1F61D #X1F61E #X1F61F #X1F620 #X1F621 #X1F622 #X1F623 #X1F624 #X1F625 #X1F626
     #X1F627 #X1F628 #X1F629 #X1F62A #X1F62B #X1F62C #X1F62D #X1F62E #X1F62F #X1F630
     #X1F631 #X1F632 #X1F633 #X1F634 #X1F635 #X1F636 #X1F637 #X1F638 #X1F639 #X1F63A
     #X1F63B #X1F63C #X1F63D #X1F63E #X1F63F #X1F640 #X1F641 #X1F642 #X1F643 #X1F644
     #X1F645 #X1F646 #X1F647 #X1F648 #X1F649 #X1F64A #X1F64B #X1F64C #X1F64D #X1F64E
     #X1F64F #X1F680 #X1F681 #X1F682 #X1F683 #X1F684 #X1F685 #X1F686 #X1F687 #X1F688
     #X1F689 #X1F68A #X1F68B #X1F68C #X1F68D #X1F68E #X1F68F #X1F690 #X1F691 #X1F692
     #X1F693 #X1F694 #X1F695 #X1F696 #X1F697 #X1F698 #X1F699 #X1F69A #X1F69B #X1F69C
     #X1F69D #X1F69E #X1F69F #X1F6A0 #X1F6A1 #X1F6A2 #X1F6A3 #X1F6A4 #X1F6A5 #X1F6A6
     #X1F6A7 #X1F6A8 #X1F6A9 #X1F6AA #X1F6AB #X1F6AC #X1F6AD #X1F6AE #X1F6AF #X1F6B0
     #X1F6B1 #X1F6B2 #X1F6B3 #X1F6B4 #X1F6B5 #X1F6B6 #X1F6B7 #X1F6B8 #X1F6B9 #X1F6BA
     #X1F6BB #X1F6BC #X1F6BD #X1F6BE #X1F6BF #X1F6C0 #X1F6C1 #X1F6C2 #X1F6C3 #X1F6C4
     #X1F6C5 #X1F6CB #X1F6CC #X1F6CD #X1F6CE #X1F6CF #X1F6D0 #X1F6D1 #X1F6D2 #X1F6D5
     #X1F6D6 #X1F6D7 #X1F6E0 #X1F6E1 #X1F6E2 #X1F6E3 #X1F6E4 #X1F6E5 #X1F6E9 #X1F6EB
     #X1F6EC #X1F6F0 #X1F6F3 #X1F6F4 #X1F6F5 #X1F6F6 #X1F6F7 #X1F6F8 #X1F6F9 #X1F6FA
     #X1F6FB #X1F6FC #X1F7E0 #X1F7E1 #X1F7E2 #X1F7E3 #X1F7E4 #X1F7E5 #X1F7E6 #X1F7E7
     #X1F7E8 #X1F7E9 #X1F7EA #X1F7EB #X1F90C #X1F90D #X1F90E #X1F90F #X1F910 #X1F911
     #X1F912 #X1F913 #X1F914 #X1F915 #X1F916 #X1F917 #X1F918 #X1F919 #X1F91A #X1F91B
     #X1F91C #X1F91D #X1F91E #X1F91F #X1F920 #X1F921 #X1F922 #X1F923 #X1F924 #X1F925
     #X1F926 #X1F927 #X1F928 #X1F929 #X1F92A #X1F92B #X1F92C #X1F92D #X1F92E #X1F92F
     #X1F930 #X1F931 #X1F932 #X1F933 #X1F934 #X1F935 #X1F936 #X1F937 #X1F938 #X1F939
     #X1F93A #X1F93C #X1F93D #X1F93E #X1F93F #X1F940 #X1F941 #X1F942 #X1F943 #X1F944
     #X1F945 #X1F947 #X1F948 #X1F949 #X1F94A #X1F94B #X1F94C #X1F94D #X1F94E #X1F94F
     #X1F950 #X1F951 #X1F952 #X1F953 #X1F954 #X1F955 #X1F956 #X1F957 #X1F958 #X1F959
     #X1F95A #X1F95B #X1F95C #X1F95D #X1F95E #X1F95F #X1F960 #X1F961 #X1F962 #X1F963
     #X1F964 #X1F965 #X1F966 #X1F967 #X1F968 #X1F969 #X1F96A #X1F96B #X1F96C #X1F96D
     #X1F96E #X1F96F #X1F970 #X1F971 #X1F972 #X1F973 #X1F974 #X1F975 #X1F976 #X1F977
     #X1F978 #X1F97A #X1F97B #X1F97C #X1F97D #X1F97E #X1F97F #X1F980 #X1F981 #X1F982
     #X1F983 #X1F984 #X1F985 #X1F986 #X1F987 #X1F988 #X1F989 #X1F98A #X1F98B #X1F98C
     #X1F98D #X1F98E #X1F98F #X1F990 #X1F991 #X1F992 #X1F993 #X1F994 #X1F995 #X1F996
     #X1F997 #X1F998 #X1F999 #X1F99A #X1F99B #X1F99C #X1F99D #X1F99E #X1F99F #X1F9A0
     #X1F9A1 #X1F9A2 #X1F9A3 #X1F9A4 #X1F9A5 #X1F9A6 #X1F9A7 #X1F9A8 #X1F9A9 #X1F9AA
     #X1F9AB #X1F9AC #X1F9AD #X1F9AE #X1F9AF #X1F9B0 #X1F9B1 #X1F9B2 #X1F9B3 #X1F9B4
     #X1F9B5 #X1F9B6 #X1F9B7 #X1F9B8 #X1F9B9 #X1F9BA #X1F9BB #X1F9BC #X1F9BD #X1F9BE
     #X1F9BF #X1F9C0 #X1F9C1 #X1F9C2 #X1F9C3 #X1F9C4 #X1F9C5 #X1F9C6 #X1F9C7 #X1F9C8
     #X1F9C9 #X1F9CA #X1F9CB #X1F9CD #X1F9CE #X1F9CF #X1F9D0 #X1F9D1 #X1F9D2 #X1F9D3
     #X1F9D4 #X1F9D5 #X1F9D6 #X1F9D7 #X1F9D8 #X1F9D9 #X1F9DA #X1F9DB #X1F9DC #X1F9DD
     #X1F9DE #X1F9DF #X1F9E0 #X1F9E1 #X1F9E2 #X1F9E3 #X1F9E4 #X1F9E5 #X1F9E6 #X1F9E7
     #X1F9E8 #X1F9E9 #X1F9EA #X1F9EB #X1F9EC #X1F9ED #X1F9EE #X1F9EF #X1F9F0 #X1F9F1
     #X1F9F2 #X1F9F3 #X1F9F4 #X1F9F5 #X1F9F6 #X1F9F7 #X1F9F8 #X1F9F9 #X1F9FA #X1F9FB
     #X1F9FC #X1F9FD #X1F9FE #X1F9FF #X1FA70 #X1FA71 #X1FA72 #X1FA73 #X1FA74 #X1FA78
     #X1FA79 #X1FA7A #X1FA80 #X1FA81 #X1FA82 #X1FA83 #X1FA84 #X1FA85 #X1FA86 #X1FA90
     #X1FA91 #X1FA92 #X1FA93 #X1FA94 #X1FA95 #X1FA96 #X1FA97 #X1FA98 #X1FA99 #X1FA9A
     #X1FA9B #X1FA9C #X1FA9D #X1FA9E #X1FA9F #X1FAA0 #X1FAA1 #X1FAA2 #X1FAA3 #X1FAA4
     #X1FAA5 #X1FAA6 #X1FAA7 #X1FAA8 #X1FAB0 #X1FAB1 #X1FAB2 #X1FAB3 #X1FAB4 #X1FAB5
     #X1FAB6 #X1FAC0 #X1FAC1 #X1FAC2 #X1FAD0 #X1FAD1 #X1FAD2 #X1FAD3 #X1FAD4 #X1FAD5
     #X1FAD6 #X203C #X2049 #X2122 #X2139 #X2194 #X2195 #X2196 #X2197 #X2198 #X2199
     #X21A9 #X21AA #X231A #X231B #X2328 #X23CF #X23E9 #X23EA #X23EB #X23EC #X23ED
     #X23EE #X23EF #X23F0 #X23F1 #X23F2 #X23F3 #X23F8 #X23F9 #X23FA #X24C2 #X25AA
     #X25AB #X25B6 #X25C0 #X25FB #X25FC #X25FD #X25FE #X2600 #X2601 #X2602 #X2603
     #X2604 #X260E #X2611 #X2614 #X2615 #X2618 #X261D #X2620 #X2622 #X2623 #X2626
     #X262A #X262E #X262F #X2638 #X2639 #X263A #X2640 #X2642 #X2648 #X2649 #X264A
     #X264B #X264C #X264D #X264E #X264F #X2650 #X2651 #X2652 #X2653 #X265F #X2660
     #X2663 #X2665 #X2666 #X2668 #X267B #X267E #X267F #X2692 #X2693 #X2694 #X2695
     #X2696 #X2697 #X2699 #X269B #X269C #X26A0 #X26A1 #X26A7 #X26AA #X26AB #X26B0
     #X26B1 #X26BD #X26BE #X26C4 #X26C5 #X26C8 #X26CE #X26CF #X26D1 #X26D3 #X26D4
     #X26E9 #X26EA #X26F0 #X26F1 #X26F2 #X26F3 #X26F4 #X26F5 #X26F7 #X26F8 #X26F9
     #X26FA #X26FD #X2702 #X2705 #X2708 #X2709 #X270A #X270B #X270C #X270D #X270F
     #X2712 #X2714 #X2716 #X271D #X2721 #X2728 #X2733 #X2734 #X2744 #X2747 #X274C
     #X274E #X2753 #X2754 #X2755 #X2757 #X2763 #X2764 #X2795 #X2796 #X2797 #X27A1
     #X27B0 #X27BF #X2934 #X2935 #X2B05 #X2B06 #X2B07 #X2B1B #X2B1C #X2B50 #X2B55
     #X3030 #X303D #X3297 #X3299))))


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
    'ido-rgrep))


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


;; =======
;; ibuffer
;; =======


(setq ibuffer-expert t)


(setq ibuffer-default-sorting-mode 'alphabetic)


(setq ibuffer-show-empty-filter-groups nil)


(defun ibuffer-custom-setup ()
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


(progn (require 'ox-md)
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
   (python . t)
   (lisp . t)))


;; cartesian product from org tables


(defun cartesian-product (factors)
  "Example:
=> (cartesian-product '((a1 a2 a3) (b1 b2) (c1)))
=> ((a1 b1 c1)
    (a1 b2 c1)
    (a2 b1 c1)
    (a2 b2 c1)
    (a3 b1 c1)
    (a3 b2 c1))"
  (if (cdr factors)
      (let ((factor (car factors))
            (vectors (cartesian-product (cdr factors))))
        (mapcan
         (lambda (f) (mapcar (lambda (v) (cons f v)) vectors))
         factor))
    (mapcar #'list (car factors))))


(defun org-table-cartesian-product ()
  "Replace current table with cartesian product of its factors
Example input:
| A  | B  | C  |
|----+----+----|
| a1 | b1 | c1 |
| a2 | b2 | c2 |
| a3 |    |    |"
  (interactive)
  (when (org-table-p)
    (let* ((table (remove 'hline (org-table-to-lisp)))
           (header (cons "#" (car table)))
           (rows (cdr table))
           (factors (apply #'cl-mapcar
                           (lambda (&rest xs)
                             (cl-remove-if (lambda (x) (equal x "")) xs))
                           rows))
           (cp (cartesian-product factors))
           (table-cartesian-product
            (append (cl-list* 'hline header 'hline
                              (cl-mapcar #'cons
                                         (cl-loop for i from 1 upto (length cp)
                                                  collect i)
                                         cp))
                    '(hline))))
      (delete-region (org-table-begin) (org-table-end))
      (insert (format "%s\n" (orgtbl-to-orgtbl
                              table-cartesian-product
                              nil)))
      (goto-char (1- (org-table-end))))))


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
;; comint-mode
;; ===========


;; use vertical tab (b) as separator in history file
;; to enable correct saving of multiline commands


(setq comint-input-ring-separator "

")


;; persistent history


(setq comint-input-ring-size 1500)


(defun comint-make-input-ring-file-name (histfile-id)
  (expand-file-name (format ".%s-history" histfile-id)
                    user-emacs-directory))


(defun comint-setup-persistent-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (let ((histfile-id (downcase (replace-regexp-in-string
                                    "<.*>\\| .+\\|[^a-zA-Z]" ""
                                    (process-name process)))))
        (setq-local comint-input-ring-file-name
                    (comint-make-input-ring-file-name histfile-id))
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


;; add useful keybindings


(define-key comint-mode-map (kbd "C-c C-i") 'comint-quit-subjob)
(define-key comint-mode-map (kbd "C-c C-k") 'comint-kill-subjob)


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


;; =====
;; shell
;; =====


(require 'shell)


(defun run-shell (preset-name &optional buffer-name)
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
         (buffer-name (or buffer-name
                          (generate-new-buffer-name
                           (format "*%s*" preset-name))))
         (wd (or (alist-get 'working-directory
                            shell-options)
                 (ido-read-directory-name "wd: "))))
    (let ((w (cl-find-if (lambda (x) (equal (window-buffer x)
                                            (get-buffer buffer-name)))
                         (window-list))))
      (when w (select-window w)))
    (switch-to-buffer buffer-name)
    (cd wd)
    (when (get-buffer-process (current-buffer))
      (comint-kill-subjob)
      (sit-for 1))
    (if startup-fn
        (funcall startup-fn buffer-name)
      (let ((explicit-shell-file-name (alist-get 'file-name shell-options)))
        (shell buffer-name)))
    (when codings
      (set-buffer-process-coding-system (car codings) (cadr codings)))
    ;; Enable restart
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key
     (kbd "C-c C-j")
     `(lambda () (interactive)
        (comint-save-history)
        (run-shell ,preset-name (buffer-name))))))


(setq shell-presets
      ;; Default preset, same as M-x shell
      '(("shell")))


;; enable restarting of async shell commands


(defun command-to-buffer-name (command)
  (let ((max-chars 80))
    (format "*%s*"
            (if (> (length command) max-chars)
                (format "%s[...]" (substring command 0 max-chars))
              command))))


(defun async-shell-command-setup-restart (f &rest args)
  (let* ((r (apply f args))
         (b (if (windowp r)
                (window-buffer r)
              (process-buffer r)))
         (command (car args)))
    (with-current-buffer b
      (setq-local shell-last-command command)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key
       (kbd "C-c C-j")
       `(lambda () (interactive)
          (let* ((command (read-shell-command "Command: " shell-last-command))
                 (buffer (current-buffer))
                 (name (command-to-buffer-name command)))
            (when (get-buffer-process buffer)
              (comint-kill-subjob)
              (sit-for 1))
            (comint-save-history)
            (unless (string-equal command shell-last-command)
              (rename-buffer name))
            (async-shell-command command buffer)))))
    r))


(advice-add 'async-shell-command :around 'async-shell-command-setup-restart)


;; more descriptive names for 'Async shell command' buffers


(defun async-shell-command-setup-sensible-name (f &rest args)
  (let* ((command (car args))
         (buffer-name (or (cadr args)
                          (command-to-buffer-name command))))
    (apply f command buffer-name (cddr args))))


(advice-add 'async-shell-command :around 'async-shell-command-setup-sensible-name)


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
         (rpt (sql-make-progress-reporter nil "Login"))
         (update-db-files (when (boundp 'sql-db-copies)
                            (let ((sql-database-original (car sql-db-copies))
                                  (sql-database-copy (cadr sql-db-copies)))
                              (when (file-exists-p sql-database-copy)
                                (let ((c (read-key (format "What to do with temp file \"%s\"?\n[P]ush to remote host\n[any other key] - Delete it, pull db from remote host (if any)"
                                                           sql-database-copy))))
                                  `(lambda ()
                                     (cond ((char-equal ,c ?p)
                                            (copy-file ,sql-database-copy
                                                       ,sql-database-original
                                                       t)
                                            (format "push working copy: %s -> %s"
                                                    ,sql-database-copy
                                                    ,sql-database-original))
                                           ((file-exists-p ,sql-database-original)
                                            (copy-file ,sql-database-original
                                                       ,sql-database-copy
                                                       t t)
                                            (format "pull remote db: %s -> %s"
                                                    ,sql-database-original
                                                    ,sql-database-copy))
                                           (t (delete-file ,sql-database-copy)
                                              (format "delete local copy: %s"
                                                      ,sql-database-copy))))))))))
    (comint-save-history) ;; save current command history
    (setq-local comint-preoutput-filter-functions
                (default-value 'comint-preoutput-filter-functions)) ;; force reset comint-preoutput-filter-functions
    (process-send-eof) ;; shutdown sql interpreter
    (sit-for 2) ;; pause for a while (ugly hack)
    (when update-db-files
      (let ((m (funcall update-db-files))
            (pattern "
Process .+

"))
        (replace-regexp pattern (format "-- %s\n" m) nil nil nil t)))
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


(define-key sql-interactive-mode-map (kbd "C-c C-j") 'sql-reconnect)


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
    ("hw" "public class Main {
    public static void main(String[] args) {
        System.out.println(\"Hello world!\");
    }
}")
    ("sout" "System.out.println(\"\");")
    ("sof" "System.out.printf(\"\");")
    ("sf" "String.format(\"\");")
    ("fori" "for (int i = 0; i < 42; i++) {\n\n}")))


;; =======
;; clojure
;; =======


(setq cider-repl-history-file
      (expand-file-name ".cider-history" user-emacs-directory))


(setq cider-show-error-buffer nil)


;; ==========
;; emacs lisp
;; ==========


(defun insert-buffer-name (buffer-name)
  (interactive (list (ido-completing-read
                      "Insert buffer name: "
                      (mapcar #'buffer-name (buffer-list)))))
  (insert buffer-name))


(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("wcb" "(with-current-buffer \"\")")))


;; evaluating stuff


(defun elisp-eval-region-or-buffer ()
  (interactive)
  (if (use-region-p)
      (let ((s (region-beginning))
            (e (region-end)))
        (message "Evaluating region: (%d, %d)" s e)
        (eval-region s e))
    (progn (message "Evaluating buffer: %s" (current-buffer))
           (eval-buffer))))


;; ===========
;; common lisp
;; ===========


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
          (message "Using venv: %s" venv))
      (run-python))))


(define-key python-mode-map (kbd "C-c C-p") 'run-python-with-venv)


(define-abbrev-table 'python-mode-abbrev-table
  '(("main" "if __name__ == '__main__':\n")
    ("fori" "for x in xs:\n")))


;; ==========
;; powershell
;; ==========


(defun powershell-setup-histfile (f &rest args)
  (let ((buffer (apply f args)))
    (with-current-buffer buffer
      (setq-local comint-input-ring-file-name
                  (comint-make-input-ring-file-name "powershell"))
      (comint-read-input-ring t))))


(advice-add 'powershell :around 'powershell-setup-histfile)


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


;; ===
;; eww
;; ===


(with-eval-after-load 'eww
  ;; Setup download directory
  (when system-type-is-windows
    (setq eww-download-directory
          (expand-file-name "Downloads"
                            (getenv "USERPROFILE")))))


(with-eval-after-load 'shr
  ;; Use monospaced fonts by default
  (setq shr-use-fonts nil)
  (setq shr-inhibit-images t))


;; ===========================
;; load site-specific settings
;; ===========================


(let ((site-file (expand-file-name "site.el" user-emacs-directory)))
  (when (file-exists-p site-file)
    (load-file site-file)))
