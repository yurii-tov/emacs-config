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


(when system-type-is-windows
  (set-coding-system-priority 'cp1251-dos))


(prefer-coding-system 'utf-8-unix)


(setq default-input-method 'russian-computer)


;; shutdown


(setq confirm-kill-processes nil
      confirm-kill-emacs 'y-or-n-p)


;; built-in history facilities


(savehist-mode 1)


(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      history-delete-duplicates t)


;; third-party packages


(require 'package)


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))


(package-initialize)


(let ((packages '(zenburn-theme
                  cider
                  slime
                  groovy-mode
                  htmlize
                  clojure-mode
                  powershell
                  ido-vertical-mode
                  base16-theme))
      refreshed)
  (dolist (p packages)
    (unless (package-installed-p p)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install p))))


;; file for auto-saving customization data


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(when (file-exists-p custom-file)
  (load-file custom-file))


;; enable unix'y things from MSYS2


(when system-type-is-windows
  (let ((msys "C:/tools/msys64"))
    (if (file-exists-p msys)
        (progn (setenv "PATH"
                       (format "%1$s\\mingw64\\bin;%1$s\\usr\\bin;%s"
                               (replace-regexp-in-string "/" "\\\\" msys)
                               (getenv "PATH")))
               (setenv "LC_ALL" "en_GB.UTF-8")
               (add-to-list 'exec-path (format "%s/usr/bin" msys))
               (add-to-list 'exec-path (format "%s/mingw64/bin" msys))
               (setq shell-file-name "bash"))
      (warn "msys2 not found. Expected location is %s" msys))))



;; ==================
;; global keybindings
;; ==================


;; unset all C-dight / M-dight combos


(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))


;; dealing with keymaps boilerplate


(defun bind-keys (keybindings &optional keymap)
  (dotimes (i (length keybindings))
    (when (zerop (mod i 2))
      (let ((key (kbd (nth i keybindings)))
            (command (nth (1+ i) keybindings)))
        (if keymap
            (define-key keymap key command)
          (local-set-key key command))))))


(defmacro define-custom-keymap (name prefix-key &rest bindings)
  `(progn (define-prefix-command ',name)
          (global-set-key (kbd ,prefix-key) ',name)
          (bind-keys ',bindings ',name)))


;; starting REPLs


(define-custom-keymap repls-map "C-c j"
                      "j" run-default-shell
                      "J" run-ssh-session
                      "s" connect-clojure-socket-repl
                      "o" run-powershell
                      "i" ielm
                      "l" slime
                      "p" run-python
                      "q" sql-connect
                      "d" docker-connect
                      "k" cider-start-map
                      "b" babashka
                      "g" run-groovy)


;; extending global search map


(bind-keys '("f" find-dired
             "g" rgrep
             "s" browse-url-or-search
             "t" translate-en-ru-online)
           search-map)


;; disable M-s overriding in all major modes


(defun disable-search-map-overriding ()
  (local-unset-key (kbd "M-s")))


(add-hook 'after-change-major-mode-hook 'disable-search-map-overriding)


;; transforming text


(define-custom-keymap text-transform-map "M-c"
                      "c" upcase-char
                      "o" sort-lines
                      "O" shuffle-lines
                      "s" replace-string
                      "r" replace-regexp
                      "u" upcase-dwim
                      "l" downcase-dwim
                      "d" delete-duplicate-lines
                      "M-c" duplicate-line
                      "q" fill-paragraph
                      "i" invert-chars
                      "j" join-region
                      "b" break-line
                      "f" flush-lines
                      "k" keep-lines
                      "e" enumerate-lines
                      "w" whitespace-cleanup
                      "," (lambda () (interactive) (wrap-with-text "[" "]"))
                      "<" (lambda () (interactive) (wrap-with-text "{" "}"))
                      "." (lambda () (interactive) (wrap-with-text "\"" "\""))
                      ">" (lambda () (interactive) (wrap-with-text "'" "'"))
                      "?" (lambda () (interactive) (wrap-with-text "<" ">"))
                      "/" (lambda () (interactive) (wrap-with-text "*" "*"))
                      "p" (lambda () (interactive) (wrap-with-text "(" ")")))


;; inserting things


(define-custom-keymap insert-map "C-x i"
                      "i" emoji-search
                      "f" insert-file
                      "n" insert-path
                      "a" insert-fortune
                      "b" insert-buffer
                      "I" insert-char)


;; diff


(define-custom-keymap diff-map "C-c d"
                      "f" diff
                      "b" diff-buffers)


;; misc


(bind-keys '("M-=" count-words
             "C-x u" reopen-with-sudo
             "C-x p" async-shell-command
             "C-x C-b" ibuffer
             "C-x l" hl-line-mode
             "C-x C-l" display-line-numbers-mode
             "M-l" (lambda () (interactive) (move-line 'up))
             "C-M-l" (lambda () (interactive) (move-line 'down))
             "C-c s" serve-directory
             "C-c e" elisp-eval-region-or-buffer
             "C-c n" make-scratch-buffer
             "C-c z" zone
             "C-c v" capture-video
             "C-c p" copy-file-name-to-clipboard
             "M-k" kill-line-to-indentation
             "M-q" hippie-expand
             "C-v" scroll-up-5-lines
             "M-v" scroll-down-5-lines
             "M-i" reformat-region
             "M-u" force-revert-buffer
             "C-c r" rename-buffer
             "C-c h" hexl-mode
             "C-c a" org-agenda
             "C-c c" org-capture
             "C-=" text-scale-increase
             "C-M-=" text-scale-decrease
             "M-o" other-window
             "C-1" delete-other-windows
             "C-2" split-window-below
             "C-3" split-window-right
             "C-0" delete-window)
           global-map)


;; =============
;; look and feel
;; =============


;; no blinking cursor


(blink-cursor-mode 0)


;; remove "welcome" messages on startup


(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;; maximize window on startup


(add-hook 'emacs-startup-hook 'toggle-frame-maximized)


;; show fortune instead of stupid default message


(defun insert-scratch-fortune ()
  (let* ((f (fortune)))
    (when f
      (with-current-buffer "*scratch*"
        (delete-region (point-min) (point-max))
        (insert (replace-regexp-in-string "\n" "" (emacs-version)))
        (newline 3)
        (insert f)
        (comment-region (point-min) (point-max))
        (newline 3)))))


(add-hook 'emacs-startup-hook 'insert-scratch-fortune)


;; replace annoying confirmations with less annoying


(setq use-short-answers t)


(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;; turn off bell ringing


(setq ring-bell-function 'ignore)


;; show line numbers


(dolist (x '(prog-mode-hook sgml-mode-hook conf-mode-hook))
  (add-hook x 'display-line-numbers-mode))


;; no line-wrap indicators


(when fringe-indicator-alist
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        '(nil nil)))


;; no scrollbar


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


;;;; enable emojis on Windows


(when (and system-type-is-windows
           (font-available-p "Segoe UI Emoji"))
  (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend))


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
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme theme)))


;; base16-theme


(setq base16-theme-distinct-fringe-background nil)


;;;; fix modeline contrast in some base16 themes


(require 'color)


(setq color-contrast-acceptable 3.4)


(defun relative-luminance (rgb)
  (let* ((rgb (mapcar (lambda (x)
                        (if (<= x 0.03928)
                            (/ x 12.92)
                          (expt (/ (+ 0.055 x) 1.055) 2.4)))
                      rgb)))
    (apply #'+ (cl-mapcar #'* rgb '(0.2126 0.7152 0.0722)))))


(defun color-contrast (a b)
  (let* ((rls (mapcar (lambda (x)
                        (relative-luminance
                         (color-name-to-rgb x)))
                      (list a b)))
         (ratio (/ (+ (car rls) 0.05)
                   (+ (cadr rls) 0.05))))
    (if (> ratio 1.0)
        ratio
      (/ 1.0 ratio))))


(defun base16-theme-colors (theme)
  (eval (intern (concat (symbol-name theme) "-theme-colors"))))


(defun base16-color-contrast (colors a b)
  (let* ((ab-colors (mapcar (lambda (x)
                              (plist-get colors x))
                            (list a b))))
    (apply #'color-contrast ab-colors)))


(defun base16-patch-modeline (colors)
  (let* ((bg (plist-get colors :base01))
         (fg (plist-get colors :base06)))
    (set-face-attribute 'mode-line nil
                        :foreground fg
                        :background bg)))


(defun base16-patch (f &rest args)
  (prog1 (apply f args)
    (let ((theme (car args)))
      (when (string-match "^base16-" (symbol-name (car args)))
        (let* ((colors (base16-theme-colors theme))
               (contrast (base16-color-contrast
                          colors
                          :base02
                          :base04)))
          (when (< contrast
                   color-contrast-acceptable)
            (base16-patch-modeline colors)))))))


(advice-add 'load-theme :around #'base16-patch)


;; better modeline


(require 'hi-lock)


(defun count-lwc ()
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start)))
    (propertize (format "%d line%s, %d word%s, %d char%s"
                        lines (if (= lines 1) "" "s")
                        words (if (= words 1) "" "s")
                        chars (if (= chars 1) "" "s"))
                'face 'mode-line-emphasis)))


(setq-default mode-line-format
              `((:eval (format "%s%s "
                               (symbol-name buffer-file-coding-system)
                               (if current-input-method-title
                                   (concat " " (propertize (format "[%s]" current-input-method-title) 'face 'hi-pink))
                                 "")))
                (:eval (let ((ro buffer-read-only)
                             (m (and (buffer-file-name) (buffer-modified-p))))
                         (cond ((and m ro) "🔏")
                               (ro "🔒")
                               (m "✒")
                               (t " "))))
                "  "
                mode-line-buffer-identification
                "  "
                (:eval (propertize "%l:%C" 'face 'font-lock-builtin-face))
                (:eval (when (use-region-p) (format "  %s " (count-lwc))))
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
    (ido-record-work-directory (file-name-directory file-name))
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
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil)


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun dired-open-in-external-app ()
  (interactive)
  (mapc #'open-in-external-app (dired-get-marked-files)))


(defun customize-dired-keys ()
  (bind-keys '("C-c C-o" dired-open-in-external-app
               "/" dired-hide-details-mode)))


(add-hook 'dired-mode-hook 'customize-dired-keys)


(add-hook 'dired-mode-hook 'dired-hide-details-mode)


(defun dired-custom-highlight ()
  (highlight-regexp " [0-9]+\\-[0-9][0-9]\\-[0-9][0-9] [0-9][0-9]:[0-9][0-9] "
                    'font-lock-string-face))


(add-hook 'dired-hide-details-mode-hook 'dired-custom-highlight)


;;;; set custom names for "find" buffers


(defun find-dired-setup-buffer (f &rest args)
  (let* ((working-directory (directory-file-name (car args)))
         (name (file-name-base working-directory))
         (existing (get-buffer name)))
    (when (and existing
               (equal (with-current-buffer existing
                        (directory-file-name default-directory))
                      working-directory)
               (eq (with-current-buffer existing major-mode)
                   'dired-mode))
      (kill-buffer existing))
    (apply f args)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (kill-buffer (current-buffer))
            (apply #'find-dired ',args)))
    (rename-buffer name t)))


(advice-add 'find-dired :around #'find-dired-setup-buffer)


;;;; display find-args in "find" buffers


(defun find-dired-display-args (f &rest args)
  (apply f args)
  (unless (string-empty-p find-args)
    (save-excursion
      (let ((formatting (string 10 32 32)))
        (goto-char 1)
        (end-of-line)
        (setq buffer-read-only nil)
        (insert formatting)
        (insert find-args)
        (insert formatting)
        (dotimes (i (length find-args))
          (insert "-"))
        (insert 10)
        (setq buffer-read-only t)))))


(advice-add 'find-dired :around #'find-dired-display-args)


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


;; force scp usage when copying files with dired


(defun dired-copy-force-scp (f from to &rest args)
  (apply f (append (mapcar (lambda (x) (replace-regexp-in-string "^/ssh" "/scp" x))
                           (list from to))
                   args)))


(advice-add 'dired-copy-file :around #'dired-copy-force-scp)


(defun reopen-with-sudo ()
  (interactive)
  (let ((file (or (buffer-file-name) default-directory)))
    (kill-buffer)
    (if (file-remote-p file)
        (let* ((host (file-remote-p file 'host))
               (prefix (replace-regexp-in-string
                        host (concat host "|sudo:" host) (file-remote-p file))))
          (find-file (concat prefix (file-remote-p file 'localname))))
      (find-file (concat "/sudo::" file)))))


;; better unique buffer names


(require 'uniquify)


(setq uniquify-buffer-name-style 'forward)


;; ===========
;; text editor
;; ===========


(require 'rect)


;; auto-completion with TAB


(setq tab-always-indent 'complete)


(add-to-list 'completion-styles 'initials t)


;; use spaces for indentation


(setq-default indent-tabs-mode nil)


;; overwrite selected text


(delete-selection-mode t)


;; reindent / cleanup selected region or whole buffer


(defun reformat-region (start end)
  "Reindent selected region, untabify it, cleanup whitespaces"
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (untabify start end)
  (indent-region start end)
  (whitespace-cleanup))


(defun invert-chars ()
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (list (region-beginning)
                           (region-end))
                   (list (point-min)
                         (point-max))))
         (region (apply #'buffer-substring bounds))
         (inversions (mapcan (lambda (x)
                               (when (string-match (nth 2 x) region)
                                 (list x)))
                             '(("i) /↔\\" ?i "[/\\]" ("\\" "/") ("/" "\\\\"))
                               ("j) []↔{}" ?j "[][}{]" ("[" "{") ("{" "[") ("}" "]") ("]" "}"))
                               ("k) \"↔'" ?k "['\"]" ("\"" "'") ("'" "\""))
                               ("l) []↔()" ?l "[][)(]" ("[" "(") ("(" "[") (")" "]") ("]" ")"))
                               ("h) {}↔()" ?h "[}{)(]" ("{" "(") ("(" "{") (")" "}") ("}" ")")))))
         (inversion (cdr (when inversions
                           (if (cdr inversions)
                               (let* ((c (read-char (string-join (cons "What to invert:"
                                                                       (mapcar #'car inversions))
                                                                 "\n")))
                                      (inversion (assoc c (mapcar #'cdr inversions))))
                                 inversion)
                             (cdar inversions))))))
    (when inversion
      (save-excursion
        (goto-char (car bounds))
        (while (re-search-forward (car inversion) (cadr bounds) t)
          (replace-match (cadr (assoc (buffer-substring (1- (point)) (point))
                                      (cdr inversion)
                                      #'string-equal))))))))


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
  (apply #'shell-command-on-region
         (append (if (use-region-p)
                     (list (region-beginning)
                           (region-end))
                   (list (point-min)
                         (point-max)))
                 (list "shuf" (current-buffer) t))))


(defun enumerate-lines ()
  (interactive)
  (if rectangle-mark-mode
      (call-interactively 'rectangle-number-lines)
    (let* ((bounds (if (use-region-p)
                       (list (region-beginning)
                             (region-end))
                     (list (point-min)
                           (point-max))))
           (lines (split-string (apply #'buffer-substring bounds)
                                "\n" t " *")))
      (apply #'delete-region bounds)
      (insert (string-join (cl-loop for i from 1 upto (length lines)
                                    for x in lines
                                    collect (format "%s %s" i x))
                           "\n")))))


(defun join-region ()
  (interactive)
  (if (region-active-p)
      (let ((separator (read-string "Join region with: "))
            (text (buffer-substring (region-beginning)
                                    (region-end))))
        (setq text (split-string text "\n" t " *"))
        (setq text (string-join text separator))
        (delete-active-region)
        (insert text))
    (delete-indentation)))


(defun break-line ()
  (interactive)
  (let ((separator (read-string "Break line with: "))
        (text (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (setq text (split-string text separator t))
    (setq text (string-join text "\n"))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert text)))


(defun wrap-with-text (b1 b2)
  "Wraps current word (or region) with given bracket-like strings
   (e.g. brackets/quotes/apostrophes/parens etc.).
   When rectangle selection is in effect, applies wrapping on each *line* of that selection"
  (cond (rectangle-mark-mode
         (let* ((bounds (list (region-beginning) (region-end)))
                (lines (mapcar #'string-trim-right
                               (split-string (apply #'buffer-substring bounds) "\n")))
                (col (save-excursion (goto-char (car bounds)) (current-column))))
           (apply #'delete-region bounds)
           (insert (string-join (cons (format "%s%s%s" b1 (car lines) b2)
                                      (mapcar (lambda (x)
                                                (let ((offset (min col (length x))))
                                                  (format "%s%s" (concat (substring x 0 offset)
                                                                         b1
                                                                         (substring x offset))
                                                          b2)))
                                              (cdr lines)))
                                "\n"))))
        ((region-active-p)
         (let ((s (region-beginning))
               (e (region-end)))
           (goto-char s)
           (insert b1)
           (goto-char e)
           (forward-char)
           (insert b2)))
        ((or (eobp) (looking-at "[\](){}<>*\s\n.,;:\[\"']"))
         (insert b1)
         (insert b2)
         (backward-char))
        (t (save-excursion
             (forward-word)
             (backward-word)
             (insert b1)
             (forward-word)
             (insert b2)))))


(defun move-line (direction)
  (cond ((eq direction 'up)
         (beginning-of-line)
         (transpose-lines 1)
         (dotimes (i 2)
           (previous-line 1)
           (beginning-of-line)))
        ((eq direction 'down)
         (end-of-line)
         (next-line)
         (move-line 'up)
         (end-of-line)
         (next-line)
         (beginning-of-line))))


(defun kill-line-to-indentation ()
  "Kills line, leaving cursor at indentation.
   In case of empty line, kills whole line"
  (interactive)
  (back-to-indentation)
  (when (string-match-p "^[     ]*$"
                        (buffer-substring
                         (line-beginning-position)
                         (line-end-position)))
    (beginning-of-line))
  (kill-line))


(defun ido-read-from-kill-ring (prompt)
  (ido-completing-read prompt (cl-remove-duplicates kill-ring :test #'equal)))


(advice-add 'read-from-kill-ring :override #'ido-read-from-kill-ring)


(defun make-scratch-buffer ()
  (interactive)
  (let* ((name (generate-new-buffer-name "*scratch*")))
    (switch-to-buffer name)
    (org-mode)))


;; hippie-expand


(setq hippie-expand-try-functions-list
      ;; try expand to...
      '(try-expand-dabbrev ;; thing from current buffer
        try-expand-line
        try-expand-list
        try-complete-file-name-partially ;; filename
        try-complete-file-name
        try-expand-dabbrev-visible ;; thing from visible buffers
        try-expand-dabbrev-from-kill ;; thing from kill-ring
        try-expand-whole-kill
        try-expand-all-abbrevs ;; abbrev expansion (see `list-abbrevs')
        try-expand-dabbrev-all-buffers ;; thing from all buffers
        try-expand-line-all-buffers
        try-expand-list-all-buffers))


;; disable prompt about saving abbrevs


(setq save-abbrevs nil)


(defun insert-fortune ()
  (interactive)
  (insert (fortune)))


(defun insert-path ()
  (interactive)
  (insert (read-file-name "Insert path: ")))


;; When rectangular region is selected, C-SPC activates multiline editing


(defun multiline-edit (f &rest args)
  (if rectangle-mark-mode
      (call-interactively 'string-rectangle)
    (apply f args)))


(advice-add 'set-mark-command :around 'multiline-edit)


;; ================================
;; completion frameworks (IDO etc.)
;; ================================


(progn (fido-vertical-mode)
       (ido-mode 'files)
       (ido-vertical-mode))


(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-max-work-directory-list 100)


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


(defun ido-open-in-external-app ()
  (interactive)
  (let ((fname (expand-file-name (ido-name (car ido-matches))
                                 ido-current-directory)))
    (message "Open in external app: %s" fname)
    (open-in-external-app fname)
    (minibuffer-keyboard-quit)))


(bind-keys '("C-c C-o" ido-open-in-external-app
             "M-r" ido-find-dired)
           ido-file-dir-completion-map)


(defun ido-jump-to-completions ()
  (let ((w (get-buffer-window ido-completion-buffer)))
    (when w (select-window w))))


(advice-add 'ido-complete :after #'ido-jump-to-completions)


;; Update ido-work-directory-list when visiting any file/directory


(defun ido-record-visited-wd (f &rest args)
  (let* ((r (apply f args)))
    (prog1 r
      (dolist (b (if (listp r) r (list r)))
        (with-current-buffer b
          (unless (equal (car ido-work-directory-list) default-directory)
            (ido-record-work-directory default-directory)))))))


(advice-add 'find-file-noselect :around 'ido-record-visited-wd)


;; Various "Wide find file" fixes


(defun ido-wide-find-file (&optional file)
  "Redefinition of the original ido-wide-find-file from ido.el:
   When the query is empty, all directory's files are included"
  (interactive)
  (unless file
    (let ((enable-recursive-minibuffers t))
      (setq file
            (condition-case nil
                (let ((s (read-string (concat "Wide find file: " ido-current-directory) ido-text)))
                  (if (equal s "") "*" s))
              (quit "")))))
  (when (> (length file) 0)
    (setq ido-use-merged-list t ido-try-merged-list 'wide)
    (setq ido-exit 'refresh)
    (setq ido-text-init file)
    (when (equal file "*")
      (setq ido-text-init ""))
    (setq ido-rotate-temp t)
    (exit-minibuffer)))


(defun ido-wide-find-dirs-or-files (dir file &optional prefix finddir)
  "Overrides original function. Now it:
   - Is able to search remote directories
   - Splits 'find' output with newline symbol"
  (let* ((remote-prefix (file-remote-p dir))
         (default-directory dir)
         (filenames
          (delq nil
                (mapcar (lambda (name)
                          (unless (ido-ignore-item-p name ido-ignore-files t)
                            name))
                        (split-string
                         (shell-command-to-string
                          (concat "find "
                                  (shell-quote-argument
                                   (if remote-prefix
                                       (string-remove-prefix remote-prefix dir) dir))
                                  (if ido-case-fold " -iname " " -name ")
                                  (shell-quote-argument
                                   (concat (if prefix "" "*") file "*"))
                                  " -type " (if finddir "d" "f") " -print"))
                         "\n"))))
         filename d f
         res)
    (while filenames
      (setq filename (format "%s%s" (or remote-prefix "") (car filenames))
            filenames (cdr filenames))
      (if (and (file-name-absolute-p filename)
               (file-exists-p filename))
          (setq d (file-name-directory filename)
                f (file-name-nondirectory filename)
                res (cons (cons (if finddir (ido-final-slash f t) f) d) res))))
    res))


(defun ido-get-work-directory (&optional incr must-match)
  "Overrides original function from ido.el.
   Now it filters out directories from disconnected remote hosts"
  (let ((n (length ido-work-directory-list))
        (i ido-work-directory-index)
        (j 0)
        dir)
    (if (or (not ido-text) (= (length ido-text) 0))
        (setq must-match nil))
    (while (< j n)
      (setq i (+ i incr)
            j (1+ j))
      (if (> incr 0)
          (if (>= i n) (setq i 0))
        (if (< i 0) (setq i (1- n))))
      (setq dir (nth i ido-work-directory-list))
      (if (and dir
               (not (equal dir ido-current-directory))
               (not (and (file-remote-p dir) (not (file-remote-p dir nil t))))
               (file-directory-p dir)
               (or (not must-match)
                   ;; TODO. check for nonreadable and too-big.
                   (ido-set-matches-1
                    (if (eq ido-cur-item 'file)
                        (ido-make-file-list-1 dir)
                      (ido-make-dir-list-1 dir)))))
          (setq j n)
        (setq dir nil)))
    (if dir
        (setq ido-work-directory-index i))
    dir))


;; =======
;; isearch
;; =======


(setq isearch-lazy-count t)


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


(bind-keys '("M-q" isearch-query-replace
             "M-." isearch-append-wildcard
             "C-SPC" isearch-select-search-string)
           isearch-mode-map)


;; =======
;; ibuffer
;; =======


(setq ibuffer-expert t)


(setq ibuffer-default-sorting-mode 'alphabetic)


(setq ibuffer-show-empty-filter-groups nil)


(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 16 -1)
              " " filename-and-process)))


(defun ibuffer-colorize-process-info (s)
  (let* ((process-indicator "^([^)]+)"))
    (if (string-match process-indicator s)
        (let ((s (replace-regexp-in-string
                  process-indicator
                  "•" s)))
          (set-text-properties
           0 1 '(face (:foreground "#00cc00")) s)
          s)
      s)))


(advice-add 'ibuffer-make-column-filename-and-process
            :filter-return
            #'ibuffer-colorize-process-info)


(defun ibuffer-setup-filter-groups ()
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
           ("Docs" (mode . doc-view-mode))
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
                          (mode . clojurescript-mode)
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
           ("Tramp" (name . "^\\*tramp.*$"))
           ("System" (or (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$"))))))
  (ibuffer-switch-to-saved-filter-groups "default"))


(add-hook 'ibuffer-mode-hook 'ibuffer-setup-filter-groups)


(defun ibuffer-setup-keybindings ()
  (local-unset-key (kbd "M-o")))


(add-hook 'ibuffer-mode-hook 'ibuffer-setup-keybindings)


;; Enhance ibuffer-filter-disable (i.e. "//" command) with 'switch to last filter' ability


(defun ibuffer-toggle-last-filter (f &rest args)
  "When there is no active filters, switches to last filter we used;
   Otherwise, removes filtering"
  (if ibuffer-filtering-qualifiers
      (progn (setq-local last-filter ibuffer-filtering-qualifiers)
             (apply f args))
    (when (boundp 'last-filter)
      (setq ibuffer-filtering-qualifiers last-filter)
      (ibuffer-update nil))))


(advice-add 'ibuffer-filter-disable :around 'ibuffer-toggle-last-filter)


;; ========
;; org-mode
;; ========


(progn (require 'ox-md)
       (require 'org-tempo))


(setq org-startup-truncated nil)


(setq org-agenda-files (list org-directory))


(setq org-capture-templates
      `(("w" "Work" entry
         (file+headline "work.org" "📨 inbox")
         "* TODO %?\n")
        ("f" "Fun" entry
         (file+headline "fun.org" "📨 inbox")
         "* TODO %?\n")
        ("a" "Art" entry
         (file+headline "art.org" "📨 inbox")
         "* TODO %?\n")
        ("l" "Life" entry
         (file+headline "life.org" "📨 inbox")
         "* TODO %?\n")
        ("m" "Math" entry
         (file+headline "math.org" "📨 inbox")
         "* TODO %?\n")
        ("c" "Computer" entry
         (file+headline "computer.org" "📨 inbox")
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


(define-key org-mode-map (kbd "C-c C-/") 'org-insert-checklist-status)


;; agenda


(defun try-switch-to-agenda (f &rest args)
  (let ((agenda-buffer "*Org Agenda*"))
    (if (get-buffer agenda-buffer)
        (switch-to-buffer agenda-buffer)
      (apply f args))))


(advice-add 'org-agenda :around 'try-switch-to-agenda)


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


;; allpairs integration


(defun org-table-allpairs ()
  "Replace current table with output of allpairs algorithm.
Requires allpairs executable (https://www.satisfice.com/download/allpairs)
to be available in exec-path
Example input:
| A  | B  | C  |
|----+----+----|
| a1 | b1 | c1 |
| a2 | b2 | c2 |
| a3 |    |    |"
  (interactive)
  (let ((allpairs (executable-find "allpairs")))
    (unless allpairs
      (message "Allpairs executable not found. You should get it from https://www.satisfice.com/download/allpairs and make it available in exec-path"))
    (when (and (org-table-p) allpairs)
      (let ((tsv (make-temp-file "")))
        (org-table-export tsv "orgtbl-to-tsv")
        (let* ((allpairs-output (shell-command-to-string (format "%s %s" allpairs tsv)))
               (table-string (replace-regexp-in-string
                              "\nTEST CASES\n" ""
                              (car (split-string allpairs-output "\n\n"))))
               (table-lisp (mapcar (lambda (x) (split-string x "\t"))
                                   (split-string table-string "\n"))))
          (delete-file tsv)
          (delete-region (org-table-begin) (org-table-end))
          (insert (format "%s\n" (orgtbl-to-orgtbl table-lisp nil)))
          (goto-char (1- (org-table-end))))))))


;; Preload org-mode at startup


(defun preload-org-mode ()
  (with-temp-buffer (org-mode)))


(add-hook 'emacs-startup-hook 'preload-org-mode)


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


(setq-default comint-buffer-maximum-size (expt 2 13))


(add-to-list 'comint-output-filter-functions
             'comint-truncate-buffer)


;; add useful keybindings


(bind-keys '("C-c C-i" comint-quit-subjob
             "C-c C-k" comint-kill-subjob)
           comint-mode-map)


;; browsing comint-input-ring


(defun comint-browse-command-history ()
  (interactive)
  (let* ((current-input (buffer-substring
                         (or (marker-position comint-accum-marker)
                             (process-mark (get-buffer-process (current-buffer))))
                         (point)))
         (query (unless (string-empty-p current-input)
                  (if (memq last-command '(comint-previous-input-prefixed
                                           comint-next-input-prefixed))
                      comint-matching-input-from-input-string
                    current-input)))
         (history (ring-elements comint-input-ring))
         (command (ido-completing-read
                   "Command history: "
                   history
                   nil nil query))
         (i (cl-position command history :test #'equal)))
    (setq-local comint-input-ring-index i)
    (comint-delete-input)
    (insert command)))


(bind-keys '("M-r" comint-browse-command-history) comint-mode-map)


;;;; Use prefix-style matching when scrolling through history


(defun comint-previous-input-prefixed (&optional n)
  (interactive)
  (unless (memq last-command '(comint-previous-input-prefixed
                               comint-next-input-prefixed
                               comint-history-isearch-backward-regexp))
    (setq comint-matching-input-from-input-string
          (buffer-substring
           (or (marker-position comint-accum-marker)
               (process-mark (get-buffer-process (current-buffer))))
           (point))))
  (comint-previous-matching-input
   (format "^%s.*" (regexp-quote comint-matching-input-from-input-string))
   (or n 1)))


(defun comint-next-input-prefixed ()
  (interactive)
  (comint-previous-input-prefixed -1))


(bind-keys '("M-p" comint-previous-input-prefixed
             "M-n" comint-next-input-prefixed)
           comint-mode-map)


;; =====
;; shell
;; =====


(require 'shell)


(defun read-ssh-presets ()
  (let* ((default-directory "~")
         (hosts (split-string (shell-command-to-string "c=~/.ssh/config; [ -f $c ] && sed -n -e '/Host \\*/ d' -e 's:Host ::p' $c"))))
    (mapcar
     (lambda (x)
       (let ((wd (format "/sshx:%s:~" x)))
         `(,x (file-name . "/bin/bash")
              (working-directory . ,wd))))
     hosts)))


(defun run-shell (preset &optional buffer-name)
  "M-x shell on steroids.
   Preset is a pair of (<\"preset-name\"> . <options-alist>)
   Legal values in options-alist are:
   |-------------------+------------------------------------------------|
   | option            | description                                    |
   |-------------------+------------------------------------------------|
   | file-name         | Path to shell executable                       |
   | working-directory | Working directory for shell instance           |
   |                   | Useful for defining remote shell sessions      |
   | startup-fn        | Function to call for starting the shell        |
   |                   | By default, function `shell' is used           |
   | histfile-id       | Save history to file whth specific prefix      |
   | codings           | Explicit decoding and encoding systems         |
   |                   | (List of two symbols, e.g. '(cp1251-dos utf-8) |
   |-------------------+------------------------------------------------|"
  (interactive)
  (let* ((preset-name (car preset))
         (shell-options (cdr preset))
         (startup-fn (alist-get 'startup-fn shell-options))
         (codings (alist-get 'codings shell-options))
         (histfile-id (alist-get 'histfile-id shell-options))
         (buffer-name (or buffer-name
                          (generate-new-buffer-name
                           (format "*%s*" preset-name))))
         (wd (or (alist-get 'working-directory
                            shell-options)
                 (read-directory-name (format "Run %s at: " preset-name)))))
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
    (when histfile-id
      (setq-local comint-input-ring-file-name
                  (comint-make-input-ring-file-name histfile-id))
      (comint-read-input-ring t))
    ;; Enable restart
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key
     (kbd "C-c C-j")
     `(lambda () (interactive)
        (comint-save-history)
        (run-shell ',preset (buffer-name))))))


(defun run-default-shell ()
  (interactive)
  (run-shell (list (file-name-base shell-file-name))))


(defun run-ssh-session ()
  (interactive)
  (let* ((presets (read-ssh-presets))
         (p (ido-completing-read "Run ssh session: "
                                 (mapcar #'car presets))))
    (run-shell (cons (format "ssh-%s" p)
                     (cdr (assoc p presets))))))


;; ====================
;; async shell commands
;; ====================


;; enable restarting


(defun command-to-buffer-name (command)
  (let ((max-chars 40))
    (format "*%s*"
            (if (> (length command) max-chars)
                (format "%s…" (substring command 0 max-chars))
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
          (let* ((*async-shell-command-ask-for-wd* nil)
                 (command (read-shell-command "Command: " shell-last-command))
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


;; descriptive names


(defun async-shell-command-setup-sensible-name (f &rest args)
  (let* ((command (car args))
         (buffer-name (or (cadr args)
                          (command-to-buffer-name command))))
    (apply f command buffer-name (cddr args))))


(advice-add 'async-shell-command :around 'async-shell-command-setup-sensible-name)


;; histfile


(defun async-shell-command-setup-histfile (r)
  (let ((b (if (windowp r)
               (window-buffer r)
             (process-buffer r))))
    (with-current-buffer b
      (setq-local comint-input-ring-file-name
                  (comint-make-input-ring-file-name "shell"))
      (when (zerop (ring-length comint-input-ring))
        (comint-read-input-ring t)))
    r))


(advice-add 'async-shell-command :filter-return 'async-shell-command-setup-histfile)


;; specify working directory for the command


(defvar *async-shell-command-ask-for-wd* t)


(defun async-shell-command-setup-wd (f &rest args)
  (let ((default-directory (if *async-shell-command-ask-for-wd*
                               (read-directory-name "wd: ")
                             default-directory)))
    (ido-record-work-directory default-directory)
    (apply f args)))


(advice-add 'async-shell-command :around 'async-shell-command-setup-wd)


;; output command/wd


(require 'compile)


(defun async-shell-command-setup-echo (f &rest args)
  (let* ((r (apply f args))
         (b (if (windowp r)
                (window-buffer r)
              (process-buffer r)))
         (p (get-buffer-process b)))
    (prog1 r
      (with-current-buffer b
        (let ((info (format "*** %s ***\n*** wd: %s ***\n" (car args) default-directory)))
          (goto-char 1)
          (comint-output-filter p info)
          (set-marker comint-last-input-end (point))
          (highlight-regexp (regexp-quote info) 'compilation-info))))))


(advice-add 'async-shell-command :around 'async-shell-command-setup-echo)


;; ==========
;; SQL client
;; ==========


(require 'sql)


(setq sql-connection-alist
      '((sqlilte (sql-product 'sqlite))))


(add-hook 'sql-login-hook 'orgtbl-mode)


;; initial setup


(defun sql-perform-initial-commands ()
  (let ((commands (sql-get-product-feature sql-product :init-commands)))
    (when commands
      (let ((process (get-buffer-process (current-buffer))))
        (dolist (command commands)
          (comint-send-string process command)
          (comint-send-input))
        (sit-for 0.05)
        (kill-whole-line -2)
        (comint-send-input)))))


(add-hook 'sql-login-hook 'sql-perform-initial-commands)


;; reconnect


(defun sql-setup-reconnect ()
  (let ((process (get-buffer-process (current-buffer))))
    (setq-local process-specs
                (list (process-name process)
                      (process-command process)))))


(add-hook 'sql-login-hook
          'sql-setup-reconnect)


(defun sql-reconnect ()
  "Restart sql interpreter with same parameters"
  (interactive)
  (let* ((pname (car process-specs))
         (pcommand (cadr process-specs))
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
    (when (get-buffer-process (current-buffer))
      (process-send-eof) ;; shutdown sql interpreter
      (sit-for 2)) ;; pause for a while (ugly hack)
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
                       (unwind-protect
                           (progn (when (get-buffer-process (current-buffer))
                                    (ring-insert comint-input-ring ";")
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
                                                        t))))))
                         (delete-file ,sql-database-copy)))
                    nil t))))))


(advice-add 'sql-comint :around 'sql-handle-remote-db)


;; output preprocessing


(defun make-sql-output-preprocessor (table-parser)
  "Setup sqli output preprocessing using db-specific output parser
   Features available:
   - Slurping raw output and turning it into pretty org-mode table
     This behavior is applied automatically for select statements;
     You can also force pretty-printing by using special '-- :pprint' comment at end of a statement
   - Output data to csv file using syntax '-- :out /path/to/file.csv[separator]'
     WARNING: There is data lossage on large amounts of output. (e.g. ~ >20000 records)
     Can be avoided by turning on debug-on-error 🧙"
  (let ((prettify `(lambda (text)
                     (let ((table (car (funcall ',table-parser text))))
                       (when table
                         (orgtbl-to-orgtbl (append (cons 'hline
                                                         (cons (car table)
                                                               (cons 'hline (cdr table))))
                                                   '(hline))
                                           nil))))))
    `(lambda (string)
       (let ((last-command (ring-ref comint-input-ring 0)))
         ;; Initialize output accumulator
         (when (or (not (boundp 'sql-output-accumulator))
                   (not sql-output-accumulator))
           (setq-local sql-output-accumulator
                       `((select-p ,(string-match "select .*from " last-command))
                         (pprint-p ,(string-match "--.*:pprint" last-command))
                         (out-file ,(when (string-match "--.*:out \\(.*.csv\\)" last-command)
                                      (match-string-no-properties 1 last-command)))
                         (out-separator ,(when (string-match "--.*:out .*.csv\\(.?\\)" last-command)
                                           (string (or (car (string-to-list (match-string-no-properties 1 last-command))) 44))))
                         (service-message-p ,(string-match "^\\*\\*\\* .* \\*\\*\\*$" string))
                         (payload "")
                         (time-start ,(current-time))
                         (chunks-count 0)
                         (chunks-written 0)))
           ;; Truncate output file, if needed
           (let ((out-file (cadr (assoc 'out-file sql-output-accumulator))))
             (when out-file (write-region "" nil out-file))))
         (cl-incf (cadr (assoc 'chunks-count sql-output-accumulator)))
         (if (and (or (cadr (assoc 'select-p sql-output-accumulator))
                      (cadr (assoc 'out-file sql-output-accumulator))
                      (cadr (assoc 'pprint-p sql-output-accumulator)))
                  (not (cadr (assoc 'service-message-p sql-output-accumulator))))
             (let* ((out-file (cadr (assoc 'out-file sql-output-accumulator)))
                    (out-separator (cadr (assoc 'out-separator sql-output-accumulator)))
                    (prompt-index (string-match comint-prompt-regexp string))
                    (prompt (when prompt-index (substring string prompt-index)))
                    ;; cut prompt from current output chunk, if needed
                    (string (if prompt (string-trim (replace-regexp-in-string
                                                     comint-prompt-regexp
                                                     ""
                                                     string))
                              string))
                    (payload (unless out-file
                               (setf (cadr (assoc 'payload sql-output-accumulator))
                                     (concat (cadr (assoc 'payload sql-output-accumulator)) string)))))
               (when out-file
                 (let* ((buffer (cadr (assoc 'buffer sql-output-accumulator)))
                        (body-p buffer)
                        (buffer (or buffer ""))
                        (parsed (funcall ',table-parser (string-join (list buffer string) "\n") body-p))
                        (table (car parsed))
                        (csv (string-join (mapcar (lambda (r) (string-join r out-separator)) table) "\n")))
                   (unless body-p
                     (push '(buffer nil) sql-output-accumulator))
                   (setf (cadr (assoc 'buffer sql-output-accumulator))
                         (cadr parsed))
                   (write-region (format "%s\n" csv) nil out-file body-p -1)
                   (cl-incf (cadr (assoc 'chunks-written sql-output-accumulator)))))
               ;; we have prompt in last output chunk => time to finalize
               (when prompt
                 (prog1 (let* ((time-start (cadr (assoc 'time-start sql-output-accumulator)))
                               (time-elapsed (float-time (time-since time-start))))
                          (format "%s%s%s%s"
                                  (if (string-to-list payload)
                                      (format "%s\n" (or (funcall ,prettify payload) payload))
                                    "")
                                  (format "-- Time elapsed: %fs\n" time-elapsed)
                                  (let ((chunks-count (cadr (assoc 'chunks-count sql-output-accumulator)))
                                        (chunks-written (cadr (assoc 'chunks-written sql-output-accumulator))))
                                    (if (and out-file (not (= chunks-count chunks-written)))
                                        (format "-- WARNING: Data lossage detected: total output chunks: %s, written to file: %s. Maybe should try toggle-debug-on-error\n"
                                                chunks-count
                                                chunks-written)
                                      ""))
                                  prompt))
                   (setq-local sql-output-accumulator nil))))
           (progn
             (setq-local sql-output-accumulator nil)
             string)))))) ;; else return input unchanged


(defun sql-setup-output-preprocessing ()
  (let ((table-parser (sql-get-product-feature sql-product :table-parser)))
    (when table-parser
      (ring-insert comint-input-ring ";") ;; hack for preventing influence of previous history on startup
      (setq-local comint-preoutput-filter-functions
                  (cons (car comint-preoutput-filter-functions)
                        (cons (make-sql-output-preprocessor table-parser)
                              (cdr comint-preoutput-filter-functions)))))))


(add-hook 'sql-login-hook
          'sql-setup-output-preprocessing)


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


(defun parse-isql-table (text &optional body-p)
  (unless (string-match "Dynamic SQL Error" text)
    (let* ((records-raw (split-string (string-trim text) "\n\n" t))
           (records-raw-r (reverse records-raw))
           (overlap-p (let* ((a (car records-raw-r))
                             (b (or (cadr records-raw-r) a)))
                        (apply #'< (mapcar (lambda (x) (length (split-string x "\n" t)))
                                           (list a b)))))
           (tail (if overlap-p (car records-raw-r) ""))
           (records-raw (if overlap-p (reverse (cdr records-raw-r)) records-raw))
           (records (mapcar (lambda (r) (mapcar (lambda (x) (list (replace-regexp-in-string " .+$" "" x)
                                                                  (string-trim (replace-regexp-in-string "^[^ ]+ +" "" x))))
                                                (split-string r "\n")))
                            records-raw))
           (header (mapcar #'car (car records)))
           (rows (mapcar (lambda (r) (mapcar #'cadr r)) records)))
      (list (if body-p rows (cons header rows)) tail))))


(sql-set-product-feature 'interbase :table-parser 'parse-isql-table)


;; sqlite


(add-to-list 'sql-sqlite-options "-interactive")


(sql-set-product-feature 'sqlite :init-commands '(".headers on"))


(defun parse-sqlite-table (text &optional ignored)
  (unless (string-match "^Error: " text)
    (list (mapcar (lambda (r) (mapcar #'string-trim (split-string r "|")))
                  (let ((lines (split-string text "\n" t)))
                    (if (string-match "^select .*;$" (car lines))
                        (cdr lines)
                      lines)))
          "")))


(sql-set-product-feature 'sqlite :table-parser 'parse-sqlite-table)


;; =====
;; clang
;; =====


(setq clang-format (executable-find "clang-format"))


(when clang-format
  (defun clang-pretty-print-buffer ()
    (interactive)
    (let* ((shell-file-name "sh")
           (extension (or (file-name-extension (or (buffer-file-name) ""))
                          (replace-regexp-in-string "-mode" "" (symbol-name major-mode))))
           (java-p (equal extension "java"))
           (style (if java-p
                      "'{BasedOnStyle: Chromium, ContinuationIndentWidth: 4, MaxEmptyLinesToKeep: 2}'"
                    "WebKit")))
      (shell-command-on-region (point-min)
                               (point-max)
                               (format "%s --assume-filename=.%s --style=%s"
                                       clang-format
                                       extension
                                       style)
                               (current-buffer)
                               t)
      (when java-p
        (save-excursion
          (while (re-search-forward "\\(
 *\\)->" nil t)
            (replace-match " ->\\1  ")
            (save-excursion
              (backward-up-list)
              (let ((s (point)))
                (forward-sexp)
                (indent-rigidly s (point) -3))))))))
  (defun enable-clang-pretty-print-buffer ()
    (local-set-key (kbd "C-c C-p") 'clang-pretty-print-buffer))
  (dolist (x '(c-mode-common-hook js-mode-hook))
    (add-hook x 'enable-clang-pretty-print-buffer)))


;; =====
;; ctags
;; =====


(defun create-tags-file ()
  (interactive)
  (let ((ctags (or (executable-find "ctags")
                   (error "Unable to find ctags executable in exec-path"))))
    (async-shell-command (format "time %s -eR --verbose=yes" ctags) "*ctags*")))


(define-custom-keymap ctags-keymap "M-s c"
                      "v" visit-tags-table
                      "s" select-tags-table
                      "c" create-tags-file)


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


;; reindent xml buffer with xmllint


(defun xml-pretty-print-buffer ()
  (interactive)
  (let ((shell-file-name "sh")
        (xmllint (executable-find "xmllint")))
    (when xmllint
      (shell-command-on-region (point-min)
                               (point-max)
                               (format "%s --format -" xmllint)
                               (current-buffer)
                               t)
      (reformat-region (point-min)
                       (point-max)))))


(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map (kbd "C-c C-p") 'xml-pretty-print-buffer))


;; =======
;; cc-mode
;; =======


(setq-default c-basic-offset 4)


(add-hook 'c-mode-hook
          (lambda () (c-set-style "k&r")))


(defun c-customize-keybindings ()
  (local-set-key (kbd "TAB") 'indent-for-tab-command))


(add-hook 'c-mode-common-hook 'c-customize-keybindings)


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
    ("fori" "for (int i = 0; i < 42; i++) {\n\n}")
    ("try" "try {\n\n} catch (Exception e) {\nthrow new RuntimeException(e);\n}")))


(defun copy-java-class-full-name ()
  "Copy full name of current class/interface/enum etc. in a form, suitable for import"
  (interactive)
  (let (package class-full-name)
    (save-excursion
      (goto-char 1)
      (re-search-forward "package *\\(.*\\);")
      (setq package (match-string 1))
      (search-forward "{")
      (re-search-backward "\\(class\\|enum\\|interface\\) *\\([^ \n]*\\)")
      (setq class-full-name (format "%s.%s" package (match-string 2))))
    (message class-full-name)
    (kill-new class-full-name)))


(defun java-setup-keybindings ()
  (local-set-key (kbd "C-c C-c") 'copy-java-class-full-name))


(add-hook 'java-mode-hook 'java-setup-keybindings)


;; =======
;; clojure
;; =======


(setq cider-repl-history-file
      (expand-file-name ".cider-history" user-emacs-directory))


(setq cider-show-error-buffer nil)


(defun connect-clojure-socket-repl ()
  (interactive)
  (let* ((socket (split-string (read-string "Connect to Clojure socket REPL: " "localhost:7777") ":"))
         (buffer-name (apply #'format "clojure-socket-repl:%s:%s" socket))
         (command (apply #'format "nc %s %s" socket)))
    (run-shell `(,buffer-name
                 (startup-fn . (lambda (b)
                                 (let ((*async-shell-command-ask-for-wd* nil))
                                   (async-shell-command (format "nc %s %s" ,@socket) b))))
                 (histfile-id . "clojure")))))


(defun babashka ()
  (interactive)
  (cider-jack-in-universal 3))


;; ==========
;; emacs lisp
;; ==========


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


(define-abbrev-table 'python-mode-abbrev-table
  '(("main" "if __name__ == '__main__':\n")
    ("fori" "for x in xs:\n")))


;; ==========
;; powershell
;; ==========


(defun run-powershell ()
  (interactive)
  (run-shell '("powershell"
               (startup-fn . powershell)
               (histfile-id . "powershell")
               (codings . (cp866-dos cp866-dos)))))


;; ======
;; Docker
;; ======


(defun docker-connect ()
  (interactive)
  (let* ((container (ido-completing-read "Connect to Docker container: " (split-string (shell-command-to-string "docker ps --format '{{.Names}}'") "\n" t)))
         (buffer-name (format "docker:%s" container))
         (command (format "docker exec -it %s bash" container))
         (*async-shell-command-ask-for-wd* nil))
    (async-shell-command command buffer-name)))


;; ===============================
;; Serving directories with Python
;; ===============================


(defun serve-directory ()
  (interactive)
  (let* ((socket (read-string "Start python server at: " "0.0.0.0:5555"))
         (command (apply 'format "python -m http.server -b %s %s" (split-string socket ":")))
         (buffer (format "*python-server:%s*" socket)))
    (async-shell-command command buffer)))


;; ================================
;; Access eng-rus dictionary online
;; ================================


(defun translate-en-ru-online (&optional query)
  "Translate from english to russian or vice versa (depending on query)"
  (interactive)
  (let* ((default-directory "~")
         (query (or query (read-string "Translate: " (word-at-point))))
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


;; ==========================
;; capture videos with ffmpeg
;; ==========================


(defun capture-video (file)
  (interactive "FCapture mp4 video to file: ")
  (let ((ffmpeg (or (executable-find "ffmpeg")
                    (error "Unable to find ffmpeg executable in exec-path")))
        (*async-shell-command-ask-for-wd* nil)
        (default-directory (file-name-directory (file-truename file))))
    (async-shell-command
     (format "%s -y -f gdigrab -i desktop -framerate 30 -pix_fmt yuv420p %s" ffmpeg file)
     (format "*ffmpeg capture → %s*" file))))


;; =======
;; fortune
;; =======


(setq fortune-file (expand-file-name "fortune.txt" user-emacs-directory))


(defun fortune ()
  (with-temp-buffer
    (insert-file-contents fortune-file)
    (goto-char (1+ (random (point-max))))
    (let* ((e (search-forward-regexp "^%$" nil t))
           (e (if e (- e 2) (point-max))))
      (goto-char e)
      (buffer-substring
       (+ 2 (or (search-backward-regexp "^%$" nil t) 1)) e))))


;; ============
;; web browsing
;; ============


;; eww


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


;; GUI browser


(defun browse-url-or-search (query)
  (interactive (list (read-string "URL/search query: "
                                  (when (region-active-p)
                                    (buffer-substring (region-beginning)
                                                      (region-end)))
                                  'browser-query-history)))
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if (string-match-p "^[a-zA-Z0-9]+://" query)
        (browse-url query)
      (browse-url (format "https://duckduckgo.com?q=%s" query)))))


(require 'ffap)


(setq ffap-url-fetcher
      (lambda (x)
        (browse-url-or-search x)
        (add-to-history 'browser-query-history x)))


;; ===========================
;; load site-specific settings
;; ===========================


(let ((site-file (expand-file-name "site.el" user-emacs-directory)))
  (when (file-exists-p site-file)
    (load-file site-file)))
