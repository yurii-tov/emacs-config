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


;; shutdown


(setq confirm-kill-processes nil
      confirm-kill-emacs 'y-or-n-p)


;; built-in history facilities


(savehist-mode 1)


(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


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
                  smex
                  ido-vertical-mode
                  base16-theme))
      refreshed)
  (dolist (p packages)
    (unless (package-installed-p p)
      (unless (or system-type-is-windows refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install p))))


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
  "o" run-powershell
  "i" ielm
  "l" slime
  "p" run-python
  "q" sql-connect
  "k" cider-connect
  "K" cider-jack-in
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
  "f" insert-file
  "p" insert-path
  "a" insert-fortune
  "b" insert-buffer-name
  "B" insert-buffer
  "n" insert-char
  "i" insert-unicode
  "j" insert-from-kill-ring
  "x" iso-transl-ctl-x-8-map)


;; diff


(define-custom-keymap diff-map "C-c d"
  "f" diff
  "b" diff-buffers)


;; registers


(progn
  (define-custom-keymap registers-map "M-j"
    "i" insert-register
    "j" jump-to-register
    "s s" copy-to-register
    "s w" window-configuration-to-register)
  (dolist (x (string-to-list "1234qwer"))
    (define-key registers-map (kbd (string x))
      `(lambda () (interactive)
         (if (region-active-p)
             (progn (set-register ,x (buffer-substring (region-beginning) (region-end)))
                    (deactivate-mark))
           (insert-register ,x))))))


;; misc


(bind-keys '("M-x" smex
             "M-=" count-words
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
             "M-o" other-window
             "C-1" delete-other-windows
             "C-2" split-window-below
             "C-3" split-window-right
             "C-0" delete-window)
           global-map)


;; =============
;; look and feel
;; =============


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
        (insert f)
        (comment-region (point-min) (point-max))
        (open-line 3)
        (end-of-buffer)))))


(add-hook 'emacs-startup-hook 'insert-scratch-fortune)


;; replace annoying confirmations with less annoying


(fset 'yes-or-no-p 'y-or-n-p)


(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;; turn off bell ringing


(setq ring-bell-function 'ignore)


;; show line numbers


(dolist (x '(prog-mode-hook text-mode-hook conf-mode-hook))
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
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))


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


;; insert various unicode chars


(defun unicode-describe-regions (regions)
  "Turns given unicode region specs into a human-readable list of definitions.
The specs are either a list denoting range, e.g. '(80 122), or an integer.
Example:
> (unicode-describe-regions '(60 (80 83) #x100))
> (\"< LESS-THAN SIGN\" \"P LATIN CAPITAL LETTER P\" \"Q LATIN CAPITAL LETTER Q\" \"R LATIN CAPITAL LETTER R\" \"S LATIN CAPITAL LETTER S\" \"Ā LATIN CAPITAL LETTER A WITH MACRON\")"
  (mapcan (lambda (r)
            (let ((r (if (listp r) r (list r))))
              (cl-loop for x from (car r) upto (or (cadr r) (car r))
                       collect
                       (concat (format "%s " (string x))
                               (get-char-code-property x 'name)))))
          regions))


(setq unicode-chars
      (let ((chars-grouped-by-category
             '((face-smiling #X1F600 #X1F603 #X1F604 #X1F601 #X1F606 #X1F605 #X1F923 #X1F602 #X1F642 #X1F643 #X1FAE0 #X1F609 #X1F60A #X1F607)
               (face-affection #X1F970 #X1F60D #X1F929 #X1F618 #X1F617 #X263A #X1F61A #X1F619 #X1F972)
               (face-tongue #X1F60B #X1F61B #X1F61C #X1F92A #X1F61D #X1F911)
               (face-hand #X1F917 #X1F92D #X1FAE2 #X1FAE3 #X1F92B #X1F914 #X1FAE1)
               (face-neutral-skeptical #X1F910 #X1F928 #X1F610 #X1F611 #X1F636 #X1FAE5 #X1F60F #X1F612 #X1F644 #X1F62C #X1F925)
               (face-sleepy #X1F60C #X1F614 #X1F62A #X1F924 #X1F634)
               (face-unwell #X1F637 #X1F912 #X1F915 #X1F922 #X1F92E #X1F927 #X1F975 #X1F976 #X1F974 #X1F635 #X1F92F)
               (face-hat #X1F920 #X1F973 #X1F978)
               (face-glasses #X1F60E #X1F913 #X1F9D0)
               (face-concerned #X1F615 #X1FAE4 #X1F61F #X1F641 #X2639 #X1F62E #X1F62F #X1F632 #X1F633 #X1F97A #X1F979 #X1F626 #X1F627 #X1F628 #X1F630 #X1F625 #X1F622 #X1F62D #X1F631 #X1F616 #X1F623 #X1F61E #X1F613 #X1F629 #X1F62B #X1F971)
               (face-negative #X1F624 #X1F621 #X1F620 #X1F92C #X1F608 #X1F47F #X1F480 #X2620)
               (face-costume #X1F4A9 #X1F921 #X1F479 #X1F47A #X1F47B #X1F47D #X1F47E #X1F916)
               (cat-face #X1F63A #X1F638 #X1F639 #X1F63B #X1F63C #X1F63D #X1F640 #X1F63F #X1F63E)
               (monkey-face #X1F648 #X1F649 #X1F64A)
               (emotion #X1F48B #X1F48C #X1F498 #X1F49D #X1F496 #X1F497 #X1F493 #X1F49E #X1F495 #X1F49F #X2763 #X1F494 #X2764 #X1F9E1 #X1F49B #X1F49A #X1F499 #X1F49C #X1F90E #X1F5A4 #X1F90D #X1F4AF #X1F4A2 #X1F4A5 #X1F4AB #X1F4A6 #X1F4A8 #X1F573 #X1F4A3 #X1F4AC #X1F5E8 #X1F5EF #X1F4AD #X1F4A4)
               (hand-fingers-open #X1F44B #X1F91A #X1F590 #X270B #X1F596 #X1FAF1 #X1FAF2 #X1FAF3 #X1FAF4)
               (hand-fingers-partial #X1F44C #X1F90C #X1F90F #X270C #X1F91E #X1FAF0 #X1F91F #X1F918 #X1F919)
               (hand-single-finger #X1F448 #X1F449 #X1F446 #X1F595 #X1F447 #X261D #X1FAF5)
               (hand-fingers-closed #X1F44D #X1F44E #X270A #X1F44A #X1F91B #X1F91C)
               (hands #X1F44F #X1F64C #X1FAF6 #X1F450 #X1F932 #X1F91D #X1F64F)
               (hand-prop #X270D #X1F485 #X1F933)
               (body-parts #X1F4AA #X1F9BE #X1F9BF #X1F9B5 #X1F9B6 #X1F442 #X1F9BB #X1F443 #X1F9E0 #X1FAC0 #X1FAC1 #X1F9B7 #X1F9B4 #X1F440 #X1F441 #X1F445 #X1F444 #X1FAE6)
               (person #X1F476 #X1F9D2 #X1F466 #X1F467 #X1F9D1 #X1F471 #X1F468 #X1F9D4 #X1F469 #X1F9D3 #X1F474 #X1F475)
               (person-gesture #X1F64D #X1F64E #X1F645 #X1F646 #X1F481 #X1F64B #X1F9CF #X1F647 #X1F926 #X1F937)
               (person-role #X1F46E #X1F575 #X1F482 #X1F977 #X1F477 #X1FAC5 #X1F934 #X1F478 #X1F473 #X1F472 #X1F9D5 #X1F935 #X1F470 #X1F930 #X1FAC3 #X1FAC4 #X1F931)
               (person-fantasy #X1F47C #X1F385 #X1F936 #X1F9B8 #X1F9B9 #X1F9D9 #X1F9DA #X1F9DB #X1F9DC #X1F9DD #X1F9DE #X1F9DF #X1F9CC)
               (person-activity #X1F486 #X1F487 #X1F6B6 #X1F9CD #X1F9CE #X1F3C3 #X1F483 #X1F57A #X1F574 #X1F46F #X1F9D6 #X1F9D7)
               (person-sport #X1F93A #X1F3C7 #X26F7 #X1F3C2 #X1F3CC #X1F3C4 #X1F6A3 #X1F3CA #X26F9 #X1F3CB #X1F6B4 #X1F6B5 #X1F938 #X1F93C #X1F93D #X1F93E #X1F939)
               (person-resting #X1F9D8 #X1F6C0 #X1F6CC)
               (family #X1F46D #X1F46B #X1F46C #X1F48F #X1F491 #X1F46A)
               (person-symbol #X1F5E3 #X1F464 #X1F465 #X1FAC2 #X1F463)
               (hair-style #X1F9B0 #X1F9B1 #X1F9B3 #X1F9B2)
               (animal-mammal #X1F435 #X1F412 #X1F98D #X1F9A7 #X1F436 #X1F415 #X1F9AE #X1F429 #X1F43A #X1F98A #X1F99D #X1F431 #X1F408 #X1F981 #X1F42F #X1F405 #X1F406 #X1F434 #X1F40E #X1F984 #X1F993 #X1F98C #X1F9AC #X1F42E #X1F402 #X1F403 #X1F404 #X1F437 #X1F416 #X1F417 #X1F43D #X1F40F #X1F411 #X1F410 #X1F42A #X1F42B #X1F999 #X1F992 #X1F418 #X1F9A3 #X1F98F #X1F99B #X1F42D #X1F401 #X1F400 #X1F439 #X1F430 #X1F407 #X1F43F #X1F9AB #X1F994 #X1F987 #X1F43B #X1F428 #X1F43C #X1F9A5 #X1F9A6 #X1F9A8 #X1F998 #X1F9A1 #X1F43E)
               (animal-bird #X1F983 #X1F414 #X1F413 #X1F423 #X1F424 #X1F425 #X1F426 #X1F427 #X1F54A #X1F985 #X1F986 #X1F9A2 #X1F989 #X1F9A4 #X1FAB6 #X1F9A9 #X1F99A #X1F99C)
               (animal-amphibian #X1F438)
               (animal-reptile #X1F40A #X1F422 #X1F98E #X1F40D #X1F432 #X1F409 #X1F995 #X1F996)
               (animal-marine #X1F433 #X1F40B #X1F42C #X1F9AD #X1F41F #X1F420 #X1F421 #X1F988 #X1F419 #X1F41A #X1FAB8)
               (animal-bug #X1F40C #X1F98B #X1F41B #X1F41C #X1F41D #X1FAB2 #X1F41E #X1F997 #X1FAB3 #X1F577 #X1F578 #X1F982 #X1F99F #X1FAB0 #X1FAB1 #X1F9A0)
               (plant-flower #X1F490 #X1F338 #X1F4AE #X1FAB7 #X1F3F5 #X1F339 #X1F940 #X1F33A #X1F33B #X1F33C #X1F337)
               (plant-other #X1F331 #X1FAB4 #X1F332 #X1F333 #X1F334 #X1F335 #X1F33E #X1F33F #X2618 #X1F340 #X1F341 #X1F342 #X1F343 #X1FAB9 #X1FABA)
               (food-fruit #X1F347 #X1F348 #X1F349 #X1F34A #X1F34B #X1F34C #X1F34D #X1F96D #X1F34E #X1F34F #X1F350 #X1F351 #X1F352 #X1F353 #X1FAD0 #X1F95D #X1F345 #X1FAD2 #X1F965)
               (food-vegetable #X1F951 #X1F346 #X1F954 #X1F955 #X1F33D #X1F336 #X1FAD1 #X1F952 #X1F96C #X1F966 #X1F9C4 #X1F9C5 #X1F344 #X1F95C #X1FAD8 #X1F330)
               (food-prepared #X1F35E #X1F950 #X1F956 #X1FAD3 #X1F968 #X1F96F #X1F95E #X1F9C7 #X1F9C0 #X1F356 #X1F357 #X1F969 #X1F953 #X1F354 #X1F35F #X1F355 #X1F32D #X1F96A #X1F32E #X1F32F #X1FAD4 #X1F959 #X1F9C6 #X1F95A #X1F373 #X1F958 #X1F372 #X1FAD5 #X1F963 #X1F957 #X1F37F #X1F9C8 #X1F9C2 #X1F96B)
               (food-asian #X1F371 #X1F358 #X1F359 #X1F35A #X1F35B #X1F35C #X1F35D #X1F360 #X1F362 #X1F363 #X1F364 #X1F365 #X1F96E #X1F361 #X1F95F #X1F960 #X1F961)
               (food-marine #X1F980 #X1F99E #X1F990 #X1F991 #X1F9AA)
               (food-sweet #X1F366 #X1F367 #X1F368 #X1F369 #X1F36A #X1F382 #X1F370 #X1F9C1 #X1F967 #X1F36B #X1F36C #X1F36D #X1F36E #X1F36F)
               (drink #X1F37C #X1F95B #X2615 #X1FAD6 #X1F375 #X1F376 #X1F37E #X1F377 #X1F378 #X1F379 #X1F37A #X1F37B #X1F942 #X1F943 #X1FAD7 #X1F964 #X1F9CB #X1F9C3 #X1F9C9 #X1F9CA)
               (dishware #X1F962 #X1F37D #X1F374 #X1F944 #X1F52A #X1FAD9 #X1F3FA)
               (place-map #X1F30D #X1F30E #X1F30F #X1F310 #X1F5FA #X1F5FE #X1F9ED)
               (place-geographic #X1F3D4 #X26F0 #X1F30B #X1F5FB #X1F3D5 #X1F3D6 #X1F3DC #X1F3DD #X1F3DE)
               (place-building #X1F3DF #X1F3DB #X1F3D7 #X1F9F1 #X1FAA8 #X1FAB5 #X1F6D6 #X1F3D8 #X1F3DA #X1F3E0 #X1F3E1 #X1F3E2 #X1F3E3 #X1F3E4 #X1F3E5 #X1F3E6 #X1F3E8 #X1F3E9 #X1F3EA #X1F3EB #X1F3EC #X1F3ED #X1F3EF #X1F3F0 #X1F492 #X1F5FC #X1F5FD)
               (place-religious #X26EA #X1F54C #X1F6D5 #X1F54D #X26E9 #X1F54B)
               (place-other #X26F2 #X26FA #X1F301 #X1F303 #X1F3D9 #X1F304 #X1F305 #X1F306 #X1F307 #X1F309 #X2668 #X1F3A0 #X1F6DD #X1F3A1 #X1F3A2 #X1F488 #X1F3AA)
               (transport-ground #X1F682 #X1F683 #X1F684 #X1F685 #X1F686 #X1F687 #X1F688 #X1F689 #X1F68A #X1F69D #X1F69E #X1F68B #X1F68C #X1F68D #X1F68E #X1F690 #X1F691 #X1F692 #X1F693 #X1F694 #X1F695 #X1F696 #X1F697 #X1F698 #X1F699 #X1F6FB #X1F69A #X1F69B #X1F69C #X1F3CE #X1F3CD #X1F6F5 #X1F9BD #X1F9BC #X1F6FA #X1F6B2 #X1F6F4 #X1F6F9 #X1F6FC #X1F68F #X1F6E3 #X1F6E4 #X1F6E2 #X26FD #X1F6DE #X1F6A8 #X1F6A5 #X1F6A6 #X1F6D1 #X1F6A7)
               (transport-water #X2693 #X1F6DF #X26F5 #X1F6F6 #X1F6A4 #X1F6F3 #X26F4 #X1F6E5 #X1F6A2)
               (transport-air #X2708 #X1F6E9 #X1F6EB #X1F6EC #X1FA82 #X1F4BA #X1F681 #X1F69F #X1F6A0 #X1F6A1 #X1F6F0 #X1F680 #X1F6F8)
               (hotel #X1F6CE #X1F9F3)
               (time #X231B #X23F3 #X231A #X23F0 #X23F1 #X23F2 #X1F570 #X1F55B #X1F567 #X1F550 #X1F55C #X1F551 #X1F55D #X1F552 #X1F55E #X1F553 #X1F55F #X1F554 #X1F560 #X1F555 #X1F561 #X1F556 #X1F562 #X1F557 #X1F563 #X1F558 #X1F564 #X1F559 #X1F565 #X1F55A #X1F566)
               (sky-and-weather #X1F311 #X1F312 #X1F313 #X1F314 #X1F315 #X1F316 #X1F317 #X1F318 #X1F319 #X1F31A #X1F31B #X1F31C #X1F321 #X2600 #X1F31D #X1F31E #X1FA90 #X2B50 #X1F31F #X1F320 #X1F30C #X2601 #X26C5 #X26C8 #X1F324 #X1F325 #X1F326 #X1F327 #X1F328 #X1F329 #X1F32A #X1F32B #X1F32C #X1F300 #X1F308 #X1F302 #X2602 #X2614 #X26F1 #X26A1 #X2744 #X2603 #X26C4 #X2604 #X1F525 #X1F4A7 #X1F30A)
               (event #X1F383 #X1F384 #X1F386 #X1F387 #X1F9E8 #X2728 #X1F388 #X1F389 #X1F38A #X1F38B #X1F38D #X1F38E #X1F38F #X1F390 #X1F391 #X1F9E7 #X1F380 #X1F381 #X1F397 #X1F39F #X1F3AB)
               (award-medal #X1F396 #X1F3C6 #X1F3C5 #X1F947 #X1F948 #X1F949)
               (sport #X26BD #X26BE #X1F94E #X1F3C0 #X1F3D0 #X1F3C8 #X1F3C9 #X1F3BE #X1F94F #X1F3B3 #X1F3CF #X1F3D1 #X1F3D2 #X1F94D #X1F3D3 #X1F3F8 #X1F94A #X1F94B #X1F945 #X26F3 #X26F8 #X1F3A3 #X1F93F #X1F3BD #X1F3BF #X1F6F7 #X1F94C)
               (game #X1F3AF #X1FA80 #X1FA81 #X1F3B1 #X1F52E #X1FA84 #X1F9FF #X1FAAC #X1F3AE #X1F579 #X1F3B0 #X1F3B2 #X1F9E9 #X1F9F8 #X1FA85 #X1FAA9 #X1FA86 #X2660 #X2665 #X2666 #X2663 #X265F #X1F0CF #X1F004 #X1F3B4)
               (arts-and-crafts #X1F3AD #X1F5BC #X1F3A8 #X1F9F5 #X1FAA1 #X1F9F6 #X1FAA2)
               (clothing #X1F453 #X1F576 #X1F97D #X1F97C #X1F9BA #X1F454 #X1F455 #X1F456 #X1F9E3 #X1F9E4 #X1F9E5 #X1F9E6 #X1F457 #X1F458 #X1F97B #X1FA71 #X1FA72 #X1FA73 #X1F459 #X1F45A #X1F45B #X1F45C #X1F45D #X1F6CD #X1F392 #X1FA74 #X1F45E #X1F45F #X1F97E #X1F97F #X1F460 #X1F461 #X1FA70 #X1F462 #X1F451 #X1F452 #X1F3A9 #X1F393 #X1F9E2 #X1FA96 #X26D1 #X1F4FF #X1F484 #X1F48D #X1F48E)
               (sound #X1F507 #X1F508 #X1F509 #X1F50A #X1F4E2 #X1F4E3 #X1F4EF #X1F514 #X1F515)
               (music #X1F3BC #X1F3B5 #X1F3B6 #X1F399 #X1F39A #X1F39B #X1F3A4 #X1F3A7 #X1F4FB)
               (musical-instrument #X1F3B7 #X1FA97 #X1F3B8 #X1F3B9 #X1F3BA #X1F3BB #X1FA95 #X1F941 #X1FA98)
               (phone #X1F4F1 #X1F4F2 #X260E #X1F4DE #X1F4DF #X1F4E0)
               (computer #X1F50B #X1FAAB #X1F50C #X1F4BB #X1F5A5 #X1F5A8 #X2328 #X1F5B1 #X1F5B2 #X1F4BD #X1F4BE #X1F4BF #X1F4C0 #X1F9EE)
               (light-and-video #X1F3A5 #X1F39E #X1F4FD #X1F3AC #X1F4FA #X1F4F7 #X1F4F8 #X1F4F9 #X1F4FC #X1F50D #X1F50E #X1F56F #X1F4A1 #X1F526 #X1F3EE #X1FA94)
               (book-paper #X1F4D4 #X1F4D5 #X1F4D6 #X1F4D7 #X1F4D8 #X1F4D9 #X1F4DA #X1F4D3 #X1F4D2 #X1F4C3 #X1F4DC #X1F4C4 #X1F4F0 #X1F5DE #X1F4D1 #X1F516 #X1F3F7)
               (money #X1F4B0 #X1FA99 #X1F4B4 #X1F4B5 #X1F4B6 #X1F4B7 #X1F4B8 #X1F4B3 #X1F9FE #X1F4B9)
               (mail #X2709 #X1F4E7 #X1F4E8 #X1F4E9 #X1F4E4 #X1F4E5 #X1F4E6 #X1F4EB #X1F4EA #X1F4EC #X1F4ED #X1F4EE #X1F5F3)
               (writing #X270F #X2712 #X1F58B #X1F58A #X1F58C #X1F58D #X1F4DD)
               (office #X1F4BC #X1F4C1 #X1F4C2 #X1F5C2 #X1F4C5 #X1F4C6 #X1F5D2 #X1F5D3 #X1F4C7 #X1F4C8 #X1F4C9 #X1F4CA #X1F4CB #X1F4CC #X1F4CD #X1F4CE #X1F587 #X1F4CF #X1F4D0 #X2702 #X1F5C3 #X1F5C4 #X1F5D1)
               (lock #X1F512 #X1F513 #X1F50F #X1F510 #X1F511 #X1F5DD)
               (tool #X1F528 #X1FA93 #X26CF #X2692 #X1F6E0 #X1F5E1 #X2694 #X1F52B #X1FA83 #X1F3F9 #X1F6E1 #X1FA9A #X1F527 #X1FA9B #X1F529 #X2699 #X1F5DC #X2696 #X1F9AF #X1F517 #X26D3 #X1FA9D #X1F9F0 #X1F9F2 #X1FA9C)
               (science #X2697 #X1F9EA #X1F9EB #X1F9EC #X1F52C #X1F52D #X1F4E1)
               (medical #X1F489 #X1FA78 #X1F48A #X1FA79 #X1FA7C #X1FA7A #X1FA7B)
               (household #X1F6AA #X1F6D7 #X1FA9E #X1FA9F #X1F6CF #X1F6CB #X1FA91 #X1F6BD #X1FAA0 #X1F6BF #X1F6C1 #X1FAA4 #X1FA92 #X1F9F4 #X1F9F7 #X1F9F9 #X1F9FA #X1F9FB #X1FAA3 #X1F9FC #X1FAE7 #X1FAA5 #X1F9FD #X1F9EF #X1F6D2)
               (other-object #X1F6AC #X26B0 #X1FAA6 #X26B1 #X1F5FF #X1FAA7 #X1FAAA)
               (transport-sign #X1F3E7 #X1F6AE #X1F6B0 #X267F #X1F6B9 #X1F6BA #X1F6BB #X1F6BC #X1F6BE #X1F6C2 #X1F6C3 #X1F6C4 #X1F6C5)
               (warning #X26A0 #X1F6B8 #X26D4 #X1F6AB #X1F6B3 #X1F6AD #X1F6AF #X1F6B1 #X1F6B7 #X1F4F5 #X1F51E #X2622 #X2623)
               (arrow (#X2190 #X21FF))
               (religion #X1F6D0 #X269B #X1F549 #X2721 #X2638 #X262F #X271D #X2626 #X262A #X262E #X1F54E #X1F52F)
               (zodiac #X2648 #X2649 #X264A #X264B #X264C #X264D #X264E #X264F #X2650 #X2651 #X2652 #X2653 #X26CE)
               (av-symbol #X1F500 #X1F501 #X1F502 #X25B6 #X23E9 #X23ED #X23EF #X25C0 #X23EA #X23EE #X1F53C #X23EB #X1F53D #X23EC #X23F8 #X23F9 #X23FA #X23CF #X1F3A6 #X1F505 #X1F506 #X1F4F6 #X1F4F3 #X1F4F4)
               (gender #X2640 #X2642 #X26A7)
               (math (#X2200 #X22FF) (#X2A00 #X2AFF) #xd7)
               (punctuation #X203C #X2049 #X2753 #X2754 #X2755 #X2757 #X3030)
               (currency #X1F4B1 #X1F4B2)
               (other-symbol #X2695 #X267B #X269C #X1F531 #X1F4DB #X1F530 #X2B55 #X2705 #X2611 #X2714 #X274C #X274E #X27B0 #X27BF #X303D #X2733 #X2734 #X2747 #X00A9 #X00AE #X2122)
               (keycap #X1F51F)
               (alphanum #X1F520 #X1F521 #X1F522 #X1F523 #X1F524 #X1F170 #X1F18E #X1F171 #X1F191 #X1F192 #X1F193 #X2139 #X1F194 #X24C2 #X1F195 #X1F196 #X1F17E #X1F197 #X1F17F #X1F198 #X1F199 #X1F19A #X1F201 #X1F202 #X1F237 #X1F236 #X1F22F #X1F250 #X1F239 #X1F21A #X1F232 #X1F251 #X1F238 #X1F234 #X1F233 #X3297 #X3299 #X1F23A #X1F235)
               (geometric #X1F534 #X1F7E0 #X1F7E1 #X1F7E2 #X1F535 #X1F7E3 #X1F7E4 #X26AB #X26AA #X1F7E5 #X1F7E7 #X1F7E8 #X1F7E9 #X1F7E6 #X1F7EA #X1F7EB #X2B1B #X2B1C #X25FC #X25FB #X25FE #X25FD #X25AA #X25AB #X1F536 #X1F537 #X1F538 #X1F539 #X1F53A #X1F53B #X1F4A0 #X1F518 #X1F533 #X1F532)
               (flag #X1F3C1 #X1F6A9 #X1F38C #X1F3F4 #X1F3F3))))
        (mapcan (lambda (g) (mapcar (lambda (x) (replace-regexp-in-string
                                                 "\\(^. \\)" (format "\\1%s/" (car g)) x))
                                    (unicode-describe-regions (cdr g))))
                chars-grouped-by-category)))


(defun insert-unicode ()
  (interactive)
  (insert (char-from-name
           (replace-regexp-in-string
            "^.*/" ""
            (ido-completing-read "Char: " unicode-chars)))))


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


(progn (ido-mode t)
       (ido-everywhere t)
       (ido-vertical-mode))


(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
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
         (file+headline "work.org" "inbox")
         "* TODO %?\n")
        ("f" "Fun" entry
         (file+headline "fun.org" "inbox")
         "* TODO %?\n")
        ("a" "Art" entry
         (file+headline "art.org" "inbox")
         "* TODO %?\n")
        ("l" "Life" entry
         (file+headline "life.org" "inbox")
         "* TODO %?\n")
        ("m" "Math" entry
         (file+headline "math.org" "inbox")
         "* TODO %?\n")
        ("c" "Computer" entry
         (file+headline "computer.org" "inbox")
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
   | codings           | Explicit decoding and encoding systems         |
   |                   | (List of two symbols, e.g. '(cp1251-dos utf-8) |
   |-------------------+------------------------------------------------|"
  (interactive)
  (let* ((preset-name (car preset))
         (shell-options (cdr preset))
         (startup-fn (alist-get 'startup-fn shell-options))
         (codings (alist-get 'codings shell-options))
         (buffer-name (or buffer-name
                          (generate-new-buffer-name
                           (format "*%s*" preset-name))))
         (wd (or (alist-get 'working-directory
                            shell-options)
                 (read-directory-name "wd: "))))
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


(defun run-powershell ()
  (interactive)
  (run-shell '("powershell"
               (startup-fn . powershell)
               (codings . (cp866-dos cp866-dos)))))


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
  (let* ((fortune-executable (executable-find "fortune")))
    (cond (fortune-executable
           (shell-command-to-string fortune-executable))
          ((file-exists-p fortune-file)
           (with-temp-buffer
             (insert-file-contents fortune-file)
             (goto-char (1+ (random (point-max))))
             (let* ((e (search-forward-regexp "^%$" nil t))
                    (e (if e (- e 2) (point-max))))
               (goto-char e)
               (buffer-substring
                (+ 2 (or (search-backward-regexp "^%$" nil t) 1)) e)))))))


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


(defun browse-url-or-search ()
  (interactive)
  (let ((query (read-string "URL/search query: "
                            (when (region-active-p)
                              (buffer-substring (region-beginning)
                                                (region-end)))))
        (browse-url-browser-function 'browse-url-default-browser))
    (if (string-match-p "^[a-zA-Z0-9]+://" query)
        (browse-url query)
      (browse-url (format "https://duckduckgo.com?q=%s" query)))))


;; ===========================
;; load site-specific settings
;; ===========================


(let ((site-file (expand-file-name "site.el" user-emacs-directory)))
  (when (file-exists-p site-file)
    (load-file site-file)))
