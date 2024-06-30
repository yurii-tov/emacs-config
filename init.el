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


;; monday-based weeks in calendar


(setq calendar-week-start-day 1)


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
        ("melpa" . "https://melpa.org/packages/")))


(let ((packages '(ido-vertical-mode
                  spacious-padding
                  ligature
                  ef-themes
                  company
                  rust-mode
                  markdown-mode
                  cider
                  powershell
                  groovy-mode
                  slime
                  nov))
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


;; unset all C-digit / M-digit combos


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
                      "c" toggle-char-case
                      "M-c" duplicate-dwim
                      "o" sort-lines     "M-o" shuffle-lines
                      "s" replace-string "M-s" replace-regexp
                      "l" upcase-dwim    "M-l" downcase-dwim
                      "j" join-lines     "M-j" break-line
                      "k" flush-lines    "M-k" keep-lines
                      "p" fill-region-justify
                      "u" delete-duplicate-lines
                      "i" invert-chars
                      "e" enumerate-lines
                      "r" reverse-region
                      "w" wrap-with-text)


;; inserting things


(define-custom-keymap insert-map "C-x i"
                      "j" emoji-insert
                      "e" emoji-list
                      "f" insert-file
                      "a" insert-fortune
                      "b" insert-buffer
                      "i" insert-char)


;; diff


(define-custom-keymap diff-map "C-x d"
                      "f" diff
                      "b" diff-buffers)


;; kmacro


(define-custom-keymap kmacro-map "C-x C-u"
                      "C-a" kmacro-add-counter
                      "C-c" kmacro-set-counter
                      "C-d" kmacro-delete-ring-head
                      "C-e" kmacro-edit-macro-repeat
                      "C-f" kmacro-set-format
                      "TAB" kmacro-insert-counter
                      "C-k" kmacro-end-or-call-macro-repeat
                      "C-l" kmacro-call-ring-2nd-repeat
                      "RET" kmacro-edit-macro
                      "C-n" kmacro-cycle-ring-next
                      "C-p" kmacro-cycle-ring-previous
                      "C-t" kmacro-swap-ring
                      "C-v" kmacro-view-macro-repeat
                      "SPC" kmacro-step-edit-macro
                      "b" kmacro-bind-to-key
                      "d" kmacro-redisplay
                      "e" edit-kbd-macro
                      "l" kmacro-edit-lossage
                      "n" kmacro-name-last-macro
                      "q" kbd-macro-query
                      "r" apply-macro-to-region-lines
                      "s" kmacro-start-macro
                      "x" kmacro-to-register)

;; project


(bind-keys '("SPC" project-dired
             "b" project-build
             "l" project-vcs-log)
           project-prefix-map)


;; misc


(bind-keys '("M-o" other-window
             "C-1" delete-other-windows
             "C-2" split-window-below
             "C-3" split-window-right
             "C-0" delete-window
             "M-k" kill-line-to-indentation
             "M-=" count-words
             "M-q" hippie-expand
             "C-v" scroll-up-5-lines
             "M-v" scroll-down-5-lines
             "M-1" shell-command
             "M-!" async-shell-command
             "M-2" (lambda () (interactive)
                     (insert-brackets '("\"\"" "''" "**") 134217778))
             "M-9" (lambda () (interactive) (wrap-with-text "(" ")" t))
             "M-0" (lambda () (interactive) (wrap-with-text "[" "]" t))
             "M-)" (lambda () (interactive) (wrap-with-text "{" "}" t))
             "M-i" reindent-region
             "M-u" force-revert-buffer
             "M-j" switch-to-buffer
             "M-`" shell
             "M-/" xref-find-references
             "M-?" eldoc-print-current-symbol-info
             "C-=" text-scale-increase
             "C-M-=" text-scale-decrease
             "C-+" (lambda () (interactive) (text-scale-set 0))
             "M-l" (lambda () (interactive) (move-line 'up))
             "C-M-l" (lambda () (interactive) (move-line 'down))
             "C-x b" bookmark-set
             "C-x B" bookmark-delete
             "C-x j" bookmark-jump
             "C-x u" reopen-with-sudo
             "C-x C-b" ibuffer
             "C-x l" hl-line-mode
             "C-x C-l" display-line-numbers-mode
             "C-x C-k" kill-buffer-and-window
             "C-x C-j" project-find-file
             "C-x C-p" project-build
             "C-h C-h" describe-symbol
             "C-h h" describe-symbol
             "C-c j" cider-start-map
             "C-c s" run-ssh-session
             "C-c d" serve-directory
             "C-c v" capture-video
             "C-c p" copy-file-name-to-clipboard
             "C-c h" hexl-mode
             "C-c c" org-capture
             "C-c a" org-agenda
             "C-c w" tail
             "C-c y" youtube-download)
           global-map)


;; =============
;; look and feel
;; =============


;; ligatures


(progn
  (ligature-set-ligatures
   t '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
       "\\\\" "://"
       ("=" "[!:/]?=*>?>?")
       ("=" "<<")
       ("-" "-*>?>?")
       ("-" "[|~<]<?")
       ("<" "<<")
       ("<" "<?--*")
       ("<" "==*")
       ("<" "<?=+>>?")
       ("<" "<?-+>>?")
       ("<" "[~*|:$/+]")
       ("<" "[$~*|+/]*>")
       ("<" "[=-]<")
       ("<" "!--*>?")
       ("<" "~~")
       ("<" "|||?")
       ("*" "\\*+")
       ("*" "[>/)]")
       ("." "\\.+")
       ("." "[?=-]")
       ("." "\\.<")
       ("+" ">")
       ("+" "\\++")))
  (global-ligature-mode t))


;; spacious-padding


(spacious-padding-mode 1)


;; remove noisy stuff


(progn (setq inhibit-startup-message t
             inhibit-startup-echo-area-message t
             use-short-answers t
             ring-bell-function 'ignore
             kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                               kill-buffer-query-functions))
       (when fringe-indicator-alist
         (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil)))
       (when (fboundp 'toggle-scroll-bar)
         (toggle-scroll-bar -1))
       (blink-cursor-mode 0)
       (tool-bar-mode -1)
       (menu-bar-mode -1))


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


;; line highlight indication in "programming" modes


(dolist (x '(prog-mode-hook sgml-mode-hook conf-mode-hook log-view-mode-hook))
  (add-hook x 'hl-line-mode))


;; highlight parentheses


(show-paren-mode)


;; fonts


(defun font-available-p (font-name)
  (not (null (member font-name (font-family-list)))))


(defun set-global-font (my-font)
  (interactive
   (list (completing-read
          "Set global font: "
          (cl-remove-duplicates (font-family-list) :test #'equal))))
  (set-face-attribute 'default nil :font my-font))


(defun set-buffer-font (font)
  (interactive
   (list (completing-read
          "Set buffer font: "
          (cl-remove-duplicates (font-family-list) :test #'equal))))
  (face-remap-add-relative 'default :family font))


;; enable emojis on Windows


(when (and system-type-is-windows
           (font-available-p "Segoe UI Emoji"))
  (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'append))


;; color themes


(advice-add 'load-theme
            :around
            (lambda (f &rest args)
              (interactive (list
                            (intern (completing-read "Load custom theme: "
                                                     (mapcar #'symbol-name
                                                             (custom-available-themes))))
                            nil nil))
              (dolist (theme custom-enabled-themes)
                (disable-theme theme))
              (apply f args)))


;; better modeline


(defun modeline-selection-stats ()
  (let* ((start (region-beginning))
         (end (region-end))
         (chars (- end start))
         (lines (count-lines start end)))
    (propertize (format "Sel: %d|%d" chars lines)
                'face 'mode-line-emphasis)))


(require 'compile)


(setq-default mode-line-format
              `(" "
                (:eval (if (member buffer-file-coding-system
                                   '(utf-8-unix
                                     prefer-utf-8-unix
                                     undecided-unix
                                     mule-utf-8-unix))
                           ""
                         (format "%s " (propertize
                                        (symbol-name buffer-file-coding-system)
                                        'face '(:slant italic)))))
                (:eval (if current-input-method-title
                           (format "%s " (propertize
                                          (downcase current-input-method-title)
                                          'face 'mode-line-emphasis))
                         ""))
                ,(if (and (not window-system) system-type-is-windows)
                     'mode-line-modified
                   '(:eval (let ((ro buffer-read-only)
                                 (m (and (buffer-file-name) (buffer-modified-p))))
                             (cond ((and m ro) "🔏 ")
                                   (ro "🔒 ")
                                   (m "✒ ")
                                   (t "")))))
                ,(unless (and (not window-system) system-type-is-windows)
                   '(:eval (if (get-buffer-process (current-buffer))
                               (propertize "• " 'face 'compilation-mode-line-run)
                             "")))
                ,(propertize "%b" 'face 'mode-line-buffer-id)
                ,(propertize " %l:%C" 'face 'shadow)
                (:eval (when (use-region-p) (format " %s" (modeline-selection-stats))))))


;; slower scrolling


(defun scroll-down-5-lines ()
  (interactive)
  (scroll-down-command 5))


(defun scroll-up-5-lines ()
  (interactive)
  (scroll-up-command 5))


;; =======
;; buffers
;; =======


;; fix kill-buffer-and-window "single window" case


(defun kill-buffer-and-window-fix (f &rest args)
  (if (> (length (window-list)) 1)
      (apply f args)
    (kill-buffer)))


(advice-add 'kill-buffer-and-window :around #'kill-buffer-and-window-fix)


;; When in "switch to buffer" menu, bind M-j to "make new scratch buffer" action
;; Therefore, given the switch-to-buffer keybinding is M-j,
;; we got convenient M-j M-j global shortcut


(defun switch-to-buffer-make-scratch-buffer ()
  (interactive)
  (let ((b (generate-new-buffer "*scratch*")))
    (with-current-buffer b
      (org-mode))
    (insert (buffer-name b)))
  (exit-minibuffer))


(advice-add 'read-buffer-to-switch
            :around
            (lambda (f &rest args)
              (minibuffer-with-setup-hook
                  (:append (lambda ()
                             (use-local-map (copy-keymap (current-local-map)))
                             (local-set-key
                              (kbd "M-j")
                              'switch-to-buffer-make-scratch-buffer)))
                (apply f args))))


;; better unique buffer names


(require 'uniquify)


(setq uniquify-buffer-name-style 'forward)


;; displaying working directory in buffer list


(defun shrink-path (path bound)
  (if (<= (length path) bound)
      path
    (let* ((split (file-name-split path))
           (lengths (mapcar #'length split))
           (i (1- (length split))))
      (while (and (> (length split) 2)
                  (> (+ i (apply #'+ lengths)) bound))
        (setq split (cons (car split) (cddr split))
              lengths (cons (car lengths) (cddr lengths))
              i (1- i)))
      (string-join (cons (car split) (cons "…" (cdr split)))
                   "/"))))


(defun switch-to-buffer-annotate-wd (f &rest args)
  (let* ((total-width (* (window-width) 0.75))
         (completion-extra-properties
          `(:annotation-function
            (lambda (x)
              (with-current-buffer x
                (concat " " (shrink-path (directory-file-name default-directory)
                                         (- ,total-width (length x)))))))))
    (apply f args)))


(advice-add 'read-buffer-to-switch :around #'switch-to-buffer-annotate-wd)


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
  (message "Force reverting buffer '%s'..." (buffer-name))
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


;; track file with 'tail' program


(defun tail (file)
  (interactive "fTail file: ")
  (let ((default-directory (file-name-directory file)))
    (async-shell-command (concat "tail -f " (file-relative-name file)))))


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
      dired-hide-details-hide-symlink-targets nil
      dired-kill-when-opening-new-dired-buffer t)


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun dired-open-in-external-app ()
  (interactive)
  (mapc #'open-in-external-app (dired-get-marked-files)))


(defun zip-file-p (filename)
  (string-match-p ".\\(zip\\|epub\\)$" filename))


(defun dired-archive ()
  (interactive)
  (let* ((output (file-truename (read-file-name "Add file(s) to archive: ")))
         (files (string-join (mapcar (lambda (x) (format "'%s'" (file-relative-name x)))
                                     (dired-get-marked-files))
                             " "))
         (tar-command (format "tar -vcz %s -f '%s'" files (replace-regexp-in-string
                                                           "^\\([a-zA-Z]\\):/"
                                                           "/\\1/"
                                                           output)))
         (zip-command (format "zip -r '%s' %s" output files))
         (command (if (zip-file-p output)
                      zip-command
                    tar-command)))
    (shell-command command)))


(defun dired-extract-archive ()
  (interactive)
  (let* ((output-dir (read-directory-name "Extract to: "))
         (archive (file-relative-name (car (dired-get-marked-files))))
         (tar-command (format "tar -xvzC '%s' < '%s'"
                              (replace-regexp-in-string "^\\([a-zA-Z]\\):/"
                                                        "/\\1/"
                                                        output-dir)
                              archive))
         (zip-command (format "unzip '%s' -d '%s'" archive output-dir))
         (command (if (zip-file-p archive)
                      zip-command
                    tar-command)))
    (shell-command command)))


(defun dired-flatten-directory ()
  "Run find program on selected directory"
  (interactive)
  (let ((directory (file-truename (car (dired-get-marked-files)))))
    (if (file-directory-p directory)
        (progn (kill-buffer)
               (find-dired directory ""))
      (message "This command works only for directories"))))


(defun customize-dired-keys ()
  (bind-keys '("o" dired-open-in-external-app
               "/" dired-hide-details-mode
               "l" dired-up-directory
               "a" dired-archive
               "A" dired-extract-archive
               "f" dired-flatten-directory)))


(add-hook 'dired-mode-hook 'customize-dired-keys)


(add-hook 'dired-mode-hook 'dired-hide-details-mode)


(add-hook 'dired-mode-hook 'hl-line-mode)


(defun dired-custom-highlight ()
  (highlight-regexp " [0-9]+\\-[0-9][0-9]\\-[0-9][0-9] [0-9][0-9]:[0-9][0-9] "
                    'font-lock-string-face))


(add-hook 'dired-hide-details-mode-hook 'dired-custom-highlight)


(defun dired-disable-ffap ()
  (setq-local ido-use-filename-at-point nil))


(add-hook 'dired-mode-hook 'dired-disable-ffap)


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


;; ===========
;; text editor
;; ===========


(require 'rect)


;; use spaces for indentation


(setq-default indent-tabs-mode nil)


;; overwrite selected text


(delete-selection-mode t)


;; more informative char inspector


(setq what-cursor-show-names t)


;; duplicating text


(setq duplicate-line-final-position -1)


;; reindenting / cleaning up


(defun reindent-region (start end)
  "Reindent selected region, untabify it, cleanup whitespaces"
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (untabify start end)
  (indent-region start end)
  (whitespace-cleanup))


;; making text nicely formatted, like in books


(setq-default fill-column 80)


(defun fill-region-justify (start end)
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (fill-region start end 'full))


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


(defun join-lines ()
  (interactive)
  (if (use-region-p)
      (let ((separator (read-string "Join with: "))
            (text (buffer-substring (region-beginning)
                                    (region-end))))
        (setq text (split-string text "\n" t " *"))
        (setq text (string-join text separator))
        (delete-active-region)
        (insert text))
    (progn
      (beginning-of-line)
      (re-search-backward "[^\s\n]" nil nil)
      (dotimes (i 2) (delete-blank-lines))
      (next-line)
      (delete-indentation))))


(defun break-line ()
  (interactive)
  (let ((separator (read-string "Break with: "))
        (text (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (setq text (split-string text separator t))
    (setq text (string-join text "\n"))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert text)))


(defun wrap-with-text (b1 b2 &optional lisp-style-p)
  "Wraps current word (or region) with given bracket-like strings
   (e.g. brackets/quotes/apostrophes/parens etc.).
   When rectangle selection is in effect, applies wrapping on each *line* of that selection"
  (interactive (let* ((s (read-string (format "Wrap with arbitrary brackets (use %s char if needed): "
                                              (propertize "?" 'face 'font-lock-builtin-face))
                                      nil nil))
                      (bs (split-string s "\\?")))
                 (if (> (length bs) 1) bs
                   (list (substring s 0 (/ (length s) 2))
                         (substring s (/ (length s) 2) (length s))))))
  (let* (b1-pos
         b2-pos
         (insert-b1 (lambda () (setq b1-pos (point)) (insert b1)))
         (insert-b2 (lambda () (setq b2-pos (point)) (insert b2))))
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
          ((use-region-p)
           (let ((s (region-beginning))
                 (e (region-end)))
             (goto-char s)
             (funcall insert-b1)
             (goto-char e)
             (forward-char (length b2))
             (funcall insert-b2)
             (when lisp-style-p (goto-char (1+ s)))))
          ((and lisp-style-p (looking-at "[\[({]"))
           (funcall insert-b1)
           (forward-sexp)
           (funcall insert-b2)
           (backward-char)
           (backward-sexp))
          ((or (eobp) (looking-at "[\](){}<>*\s\n.,;:\[\"']"))
           (funcall insert-b1)
           (funcall insert-b2)
           (backward-char))
          (t (forward-word)
             (backward-word)
             (funcall insert-b1)
             (forward-word)
             (funcall insert-b2)
             (when lisp-style-p
               (backward-char)
               (backward-word))))
    (list b1-pos b2-pos)))


(defun insert-brackets (brackets keybinding)
  (when (not rectangle-mark-mode)
    (let* ((positions (wrap-with-text (substring (car brackets) 0 1)
                                      (substring (car brackets) 1 2)
                                      t))
           (p1 (car positions))
           (p2 (cadr positions))
           (i 0)
           key)
      (while (= (setq key (read-key)) keybinding)
        (let* ((bs (nth (mod (cl-incf i) (length brackets)) brackets))
               (b1 (aref bs 0))
               (b2 (aref bs 1)))
          (save-excursion
            (goto-char p1)
            (delete-char 1)
            (insert b1)
            (goto-char p2)
            (delete-char 1)
            (insert b2))
          (forward-char)))
      (push key unread-command-events))))


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


(defun toggle-char-case ()
  (interactive)
  (save-excursion
    (if (char-uppercase-p (following-char))
        (downcase-region (point) (1+ (point)))
      (upcase-region (point) (1+ (point))))))


(defun insert-fortune ()
  (interactive)
  (insert (fortune)))


;; When rectangular region is selected, C-SPC activates multiline editing


(defun multiline-edit (f &rest args)
  (if rectangle-mark-mode
      (if current-prefix-arg
          (call-interactively 'wrap-with-text)
        (call-interactively 'string-rectangle))
    (apply f args)))


(advice-add 'set-mark-command :around 'multiline-edit)


;; enable hex mode on regions


(defun wrap-hexl-mode (f &rest args)
  (if (use-region-p)
      (shell-command-on-region
       (region-beginning)
       (region-end)
       "hexdump -C")
    (apply f args)))


(advice-add 'hexl-mode :around 'wrap-hexl-mode)


;; Enable pretty-printing "programming" modes buffers


(setq pretty-printers nil)


(defun pretty-print-buffer ()
  (interactive)
  (let ((f (cdr (assoc major-mode pretty-printers))))
    (if f
        (progn (message "Reformatting buffer...")
               (apply f nil))
      (error (format "Don't have any pprint function for %s" major-mode)))))


(defun setup-pretty-print-buffer ()
  (local-set-key (kbd "M-p") 'pretty-print-buffer))


(dolist (x '(prog-mode-hook sgml-mode-hook conf-mode-hook))
  (add-hook x 'setup-pretty-print-buffer))


;; ElDoc (documentation voewer)


(setq eldoc-echo-area-use-multiline-p nil)


;; ===========================================
;; Minibuffer completion frameworks (IDO etc.)
;; ===========================================


;; Use fido-mode as default completion method


(fido-vertical-mode)


(bind-keys '("C-j" icomplete-fido-exit
             "M-j" icomplete-force-complete
             "SPC" self-insert-command)
           icomplete-minibuffer-map)


(setq completion-auto-select t)


;; Enhance read-string with history completion


(defun read-string-completing-history (f &rest args)
  (let* ((prompt (car args))
         (initial-input (cadr args))
         (history-arg (caddr args))
         (history-symbol (cond ((consp history-arg) (car history-arg))
                               ((symbolp history-arg) history-arg)))
         (history-value (and history-symbol
                             (not (member history-symbol
                                          '(junk-hist org-read-date-history)))
                             (boundp history-symbol)
                             (symbol-value history-symbol))))
    (if (and history-value
             (listp history-value)
             (not (null history-value)))
        (completing-read prompt history-value nil nil initial-input history-symbol)
      (apply f args))))


(advice-add 'read-string :around #'read-string-completing-history)


;; Use IDO for file-browsing


(progn (ido-mode 'files)
       (ido-vertical-mode)
       (add-function :override read-file-name-function #'ido-read-file-name))


(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-max-work-directory-list 100)


;; Better IDO keybindings


(bind-keys '("M-j" ido-take-first-match
             "SPC" self-insert-command
             "M-SPC" ido-toggle-prefix
             "C-p" ido-prev-match
             "C-n" ido-next-match
             "<down>" ido-next-match
             "<up>" ido-prev-match)
           ido-common-completion-map)


(bind-keys '("<down>" ido-next-match
             "<up>" ido-prev-match)
           ido-file-dir-completion-map)


;; Ensure proper (i.e. old-fashioned) IDO fallback file-selection command:


(defun ido-disable-icomplete (f &rest args)
  (let ((icomplete-mode nil)) (apply f args)))


(progn (advice-add 'ido-file-internal :around #'ido-disable-icomplete)
       (advice-add 'ido-read-file-name :around #'ido-disable-icomplete))


(defun ido-open-in-external-app ()
  (interactive)
  (let ((fname (expand-file-name (ido-name (car ido-matches))
                                 ido-current-directory)))
    (message "Open in external app: %s" fname)
    (open-in-external-app fname)
    (minibuffer-keyboard-quit)))


(bind-keys '("C-c C-o" ido-open-in-external-app)
           ido-file-dir-completion-map)


(defun ido-insert-path ()
  (interactive)
  "Insert the (possibly relative) path of the selected file/directory"
  (let* ((fname (expand-file-name (ido-name (car ido-matches))
                                  ido-current-directory))
         (fname (or (file-remote-p fname 'localname) fname)))
    (run-with-timer
     0.1 nil
     `(lambda () (let ((path (if (string-prefix-p default-directory ,fname)
                                 (file-relative-name
                                  (substring ,fname (length default-directory)))
                               ,fname))
                       (p2 (point))
                       (p1 (save-excursion
                             (or (and (re-search-backward
                                       "\s" (line-beginning-position) t)
                                      (1+ (point)))
                                 (line-beginning-position)))))
                   (when (string-prefix-p (buffer-substring p1 p2) path)
                     (delete-region p1 p2))
                   (insert path))
        (keyboard-quit)))
    (minibuffer-keyboard-quit)))


(bind-keys '("C-v" ido-insert-path)
           ido-file-dir-completion-map)


(defun ido-jump-to-completions ()
  (let ((w (get-buffer-window ido-completion-buffer)))
    (when w (select-window w))))


(advice-add 'ido-complete :after #'ido-jump-to-completions)


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


;; =============================
;; Completion/expansion at point
;; =============================


;; Use tab for completion


(setq tab-always-indent 'complete)


(add-to-list 'completion-styles 'initials t)


;; Company (IDE-like dropdowns)


(progn (global-company-mode)
       (company-tng-mode))


(setq-default company-minimum-prefix-length 1
              company-idle-delay 0
              company-tooltip-offset-display 'lines
              company-files-chop-trailing-slash nil
              company-backends '(company-capf
                                 company-files
                                 (company-dabbrev-code
                                  company-abbrev
                                  company-keywords
                                  company-gtags
                                  company-etags)
                                 company-dabbrev))


(progn (dotimes (n 10)
         (define-key company-active-map
                     (kbd (format "M-%d" n)) nil))
       (dolist (x (list company-active-map
                        company-search-map))
         (bind-keys '("M-p" nil
                      "M-n" nil
                      "M-SPC" company-other-backend)
                    x))
       (define-key company-search-map (kbd "SPC") nil))


(defun company-setup-tab-completion ()
  (let ((m (current-local-map)))
    (when (and m
               (not buffer-read-only)
               (not (string-prefix-p "org-" (symbol-name major-mode))))
      (use-local-map (copy-keymap m))
      (local-set-key
       (kbd "<tab>")
       'company-indent-or-complete-common))))


(add-hook 'company-mode-hook 'company-setup-tab-completion)


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
        try-expand-dabbrev-all-buffers ;; thing from all buffers
        try-expand-line-all-buffers
        try-expand-list-all-buffers))


;; disable prompt about saving abbrevs


(setq save-abbrevs nil)


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


(bind-keys '("M-q" isearch-query-replace
             "M-w" isearch-toggle-word
             "C-SPC" isearch-select-search-string)
           isearch-mode-map)


;; =======
;; ibuffer
;; =======


(setq ibuffer-expert t
      ibuffer-default-sorting-mode 'alphabetic
      ibuffer-show-empty-filter-groups nil
      ibuffer-formats '((mark modified read-only locked " "
                              (name 16 -1)
                              " " filename-and-process)))


(defun ibuffer-colorize-process-info (s)
  (let* ((process-indicator "^([^)]+)"))
    (if (string-match process-indicator s)
        (let ((s (replace-regexp-in-string
                  process-indicator
                  "•⁤" s)))
          (set-text-properties
           1 2 '(ibuffer-process t) s)
          (set-text-properties
           0 1 '(face (:foreground "#00cc00")) s)
          s)
      s)))


(unless (and system-type-is-windows (not window-system))
  (advice-add 'ibuffer-make-column-filename-and-process
              :filter-return
              #'ibuffer-colorize-process-info))


(defun ibuffer-setup ()
  ;; keybindings
  (local-unset-key (kbd "M-o"))
  (local-unset-key (kbd "M-j"))
  ;; autoupdate
  (ibuffer-auto-mode 1)
  ;; filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Directory" (mode . dired-mode))
           ("Shell" (and (name . "^\\*\\([bz]?a?sh\\|powershell\\|.*-?shell\\|ssh-\\).*\\*")
                         (mode . shell-mode)))
           ("Process" (mode . shell-mode))
           ("Text" (and (not (name . "^\\*.*\\*$"))
                        (not (mode . org-mode))
                        (or (derived-mode . text-mode)
                            (mode . fundamental-mode)
                            (derived-mode . conf-mode))))
           ("Document" (or (mode . doc-view-mode)
                           (mode . nov-mode)))
           ("Org-mode" (or (mode . org-mode)
                           (name . "^\\*Org .*$")))
           ("SQL" (or (mode . sql-mode)
                      (mode . sql-interactive-mode)))
           ("C" (mode . c-mode))
           ("Java" (mode . java-mode))
           ("Groovy" (or (mode . groovy-mode)
                         (mode . inferior-groovy-mode)))
           ("Shell script" (or (mode . sh-mode)
                               (mode . bat-mode)
                               (mode . powershell-mode)))
           ("Python" (or (mode . python-mode)
                         (mode . inferior-python-mode)))
           ("Javascript" (or (mode . js-mode) (mode . js-json-mode)))
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
           ("Rust" (mode . rust-mode))
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
  (ibuffer-switch-to-saved-filter-groups "default")
  (hl-line-mode))


(add-hook 'ibuffer-mode-hook 'ibuffer-setup)


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


(setq org-startup-truncated nil
      org-adapt-indentation t)


(require 'org-tempo)


;; Capture


(setq org-capture-templates
      `(("w" "Work" entry
         (file+headline "work.org" "In")
         "* TODO %?\n")
        ("f" "Fun" entry
         (file+headline "fun.org" "In")
         "* TODO %?\n")
        ("a" "Art" entry
         (file+headline "art.org" "In")
         "* TODO %?\n")
        ("l" "Life" entry
         (file+headline "life.org" "In")
         "* TODO %?\n")
        ("m" "Math" entry
         (file+headline "math.org" "In")
         "* TODO %?\n")
        ("c" "Computer" entry
         (file+headline "computer.org" "In")
         "* TODO %?\n")))


(setq org-refile-allow-creating-parent-nodes 'confirm)


;; Export


(require 'ox-md)


(setq org-export-with-section-numbers 0
      org-export-preserve-breaks t
      org-export-with-toc nil
      org-export-with-sub-superscripts nil)


;; Checklist enhancements


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


;; Agenda


(setq org-agenda-files (list org-directory))


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


;; permutations


(defun permutations (xs)
  "Example:
=> (permutations '(a b c))
=> ((a b c)
    (a c b)
    (b a c)
    (b c a)
    (c a b)
    (c b a))"
  (when xs
    (if (cdr xs)
        (mapcan (lambda (x)
                  (mapcar (lambda (y) (cons x y))
                          (permutations (remove x xs))))
                xs)
      (list xs))))


(defun org-table-permutations ()
  "Replace one-row table with permutations of its row
Example input:
| A | B | C |
|---+---+---|
| a | b | c |"
  (interactive)
  (when (org-table-p)
    (let* ((table (remove 'hline (org-table-to-lisp)))
           (header (car table))
           (perms (permutations (cadr table)))
           (table-perms (append (cl-list* 'hline header 'hline perms)
                                '(hline))))
      (delete-region (org-table-begin) (org-table-end))
      (insert (format "%s\n" (orgtbl-to-orgtbl
                              table-perms
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


;; ==============
;; shell commands
;; ==============


;; Custom completion style for shell commands


(defun read-string-shell-command (f &rest args)
  (if shell-command-history
      (ido-completing-read
       (car args) shell-command-history nil nil (cadr args) 'shell-command-history)
    (read-string (car args) (cadr args) 'shell-command-history)))


(advice-add 'read-shell-command :around #'read-string-shell-command)


;; shell-command now operates on selected region


(defun shell-command-dwim (f &rest args)
  (if (use-region-p)
      (shell-command-on-region (region-beginning)
                               (region-end)
                               (car args)
                               nil
                               current-prefix-arg)
    (apply f args)))


(advice-add 'shell-command :around #'shell-command-dwim)


;; async-shell-command


(defun async-shell-command-read-wd (f &rest args)
  (let* ((project (project-current))
         (project-dir (when project (project-root project)))
         (default-directory (if (called-interactively-p)
                                (read-directory-name
                                 (format "Run %s at: "
                                         (propertize (reverse (string-truncate-left
                                                               (reverse (car args)) 20))
                                                     'face 'compilation-info))
                                 project-dir)
                              default-directory)))
    (apply f args)))


(advice-add 'async-shell-command :around 'async-shell-command-read-wd)


(defun async-shell-command-record-wd (f &rest args)
  (ido-record-work-directory default-directory)
  (apply f args))


(advice-add 'async-shell-command :around 'async-shell-command-record-wd)


;;;; enable restarting


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


;;;; descriptive names


(defun async-shell-command-setup-buffer-name (f &rest args)
  (let* ((command (car args))
         (buffer-name (or (cadr args)
                          (command-to-buffer-name command))))
    (apply f command buffer-name (cddr args))))


(advice-add 'async-shell-command :around 'async-shell-command-setup-buffer-name)


;;;; histfile


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


;;;; output command/wd


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
          (highlight-regexp (regexp-quote info) 'compilation-info)
          (font-lock-update))))))


(advice-add 'async-shell-command :around 'async-shell-command-setup-echo)


;;;; don't display output buffer, but promote it in switch-to-buffer menu
;;;; add some useful output to *Messages*


(defun async-shell-command-disable-popup (f &rest args)
  (let (r b)
    (save-window-excursion
      (prog1 (setq r (apply f args))
        (setq b (if (windowp r)
                    (window-buffer r)
                  (process-buffer r)))
        (switch-to-buffer b)
        (message "Running command %s at %s"
                 (propertize (car args) 'face 'compilation-info)
                 (propertize default-directory 'face 'completions-annotations))
        (when (get-buffer-process (current-buffer))
          (set-process-sentinel
           (get-process (get-buffer-process (current-buffer)))
           `(lambda (p e)
              (let ((e (string-trim-right e)))
                (unless (member ,b (mapcar #'window-buffer (window-list)))
                  (with-current-buffer ,b
                    (message "%s\n%s"
                             (buffer-substring (point-min) (point-max))
                             (propertize (format "[%s] %s" e ,(car args))
                                         'face (cond ((equal e "finished") 'success)
                                                     ((string-match "exited abnormally.*" e)
                                                      'error)
                                                     (t 'shadow))))))))))))))


(advice-add 'async-shell-command :around #'async-shell-command-disable-popup)


;; ===========
;; comint-mode
;; ===========


;; use vertical tab (b) as separator in history file
;; to enable correct saving of multiline commands


(setq comint-input-ring-separator "

")


;; persistent history


(setq comint-input-ring-size 1500)


(defvar *comint-histfile-id* nil)


(defun comint-make-input-ring-file-name (histfile-id)
  (expand-file-name (format ".%s-history" histfile-id)
                    user-emacs-directory))


(defun comint-setup-persistent-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (let ((histfile-id (or *comint-histfile-id*
                             (downcase (replace-regexp-in-string
                                        "<.*>\\| .+\\|[^a-zA-Z]" ""
                                        (process-name process))))))
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


(bind-keys '("C-c C-k" comint-kill-subjob) comint-mode-map)


;; browsing comint-input-ring


(defun comint-browse-command-history ()
  (interactive)
  (let* ((history (ring-elements comint-input-ring))
         (command (ido-completing-read "Command history: " history))
         (i (cl-position command history :test #'equal)))
    (setq-local comint-input-ring-index i)
    (comint-delete-input)
    (insert command)
    (comint-send-input)))


(bind-keys '("M-r" comint-browse-command-history) comint-mode-map)


;; Use prefix-style matching when scrolling through history


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
             "M-n" comint-next-input-prefixed
             "<up>" comint-previous-input-prefixed
             "<down>" comint-next-input-prefixed)
           comint-mode-map)


;; Company completion


(defun set-company-history-candidates ()
  (let ((prefix (replace-regexp-in-string
                 comint-prompt-regexp ""
                 (company-grab-line (concat comint-prompt-regexp ".*")))))
    (and (setq-local company-history-candidates
                     (cl-remove-if-not
                      (lambda (x) (string-prefix-p prefix x t))
                      (ring-elements comint-input-ring)))
         prefix)))


(defun company-comint-hist-completion (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-comint-history))
    (prefix (and (eobp)
                 (set-company-history-candidates)))
    (candidates company-history-candidates)
    (post-completion (let ((i (cl-position arg
                                           (ring-elements comint-input-ring)
                                           :test #'equal)))
                       (setq-local comint-input-ring-index i)))
    (sorted t)
    (duplicates t)))


(defun comint-setup-company-completion ()
  (setq-local company-backends
              '(company-comint-hist-completion
                company-capf)))


(add-hook 'comint-mode-hook 'comint-setup-company-completion)


;; Remove unneeded completion-at-point functions


(defun comint-cleanup-capf ()
  (dolist (x '(comint-c-a-p-replace-by-expanded-history
               shell-c-a-p-replace-by-expanded-directory
               shell-command-completion
               pcomplete-completions-at-point))
    (setq-local comint-dynamic-complete-functions
                (remove x comint-dynamic-complete-functions))))


(add-hook 'comint-mode-hook 'comint-cleanup-capf)


;; =====
;; shell
;; =====


(require 'shell)


;; M-x shell enhancements


(advice-add 'shell
            :around
            (lambda (f &rest args)
              (and (get-buffer "*shell*")
                   (not (get-buffer-process "*shell*"))
                   (kill-buffer "*shell*"))
              (let* ((n (if (file-remote-p default-directory)
                            "sh" (file-name-base shell-file-name)))
                     (b (or (car args) (generate-new-buffer-name (format "*%s*" n)))))
                (switch-to-buffer b)
                (apply f (cons b (cdr args)))
                (use-local-map (copy-keymap (current-local-map)))
                (local-set-key
                 (kbd "C-c C-j")
                 (lambda () (interactive)
                   (comint-save-history)
                   (when (get-buffer-process (current-buffer))
                     (comint-kill-subjob)
                     (sit-for 1))
                   (shell (buffer-name)))))))


;; Ssh sessions


(defun read-ssh-hosts ()
  (let* ((default-directory "~"))
    (split-string (shell-command-to-string "c=~/.ssh/config; [ -f $c ] && sed -n -e '/Host \\*/ d' -e 's:Host ::p' $c"))))


(defun run-ssh-session ()
  (interactive)
  (let* ((x (completing-read "Run ssh session: " (read-ssh-hosts) nil t))
         (default-directory (format "/sshx:%s:" x))
         (explicit-shell-file-name "/bin/bash"))
    (shell (format "*ssh-%s*" x))))


;; =======
;; project
;; =======


(setq project-switch-commands
      '((project-dired "Open project root")
        (project-build "Build")
        (project-find-dir "Find directory")
        (project-vcs-log "View VCS history")
        (project-shell "Shell")
        (project-find-file "Find file")
        (project-find-regexp "Find regexp")))


(defun project-vcs-log ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (let (b)
      (save-window-excursion
        (vc-print-root-log)
        (setq b (current-buffer)))
      (switch-to-buffer b))))


(setq project-build-commands nil)


(defun project-build ()
  (interactive)
  (let* ((project (project-current t))
         (default-directory (project-root project))
         (command (cdr (assoc project project-build-commands #'equal))))
    (when (or (not command) current-prefix-arg)
      (setq command (read-shell-command "Build command: "))
      (push (cons project command)
            project-build-commands))
    (async-shell-command command)))


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

;; wtf is going on?
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


(defun clang-pretty-print-buffer ()
  (interactive)
  (let* ((clang-format (or (executable-find "clang-format")
                           (error "Unable to find clang-format")))
         (shell-file-name "sh")
         (extension (or (file-name-extension (or (buffer-file-name) ""))
                        (replace-regexp-in-string "-mode" "" (symbol-name major-mode))))
         (java-p (equal extension "java"))
         (style (if java-p
                    "'{BasedOnStyle: Chromium, ContinuationIndentWidth: 4, MaxEmptyLinesToKeep: 2}'"
                  "WebKit")))
    (let ((p (point)))
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
                (indent-rigidly s (point) -3))))))
      (goto-char p))))


(dolist (x '(c-mode js-mode java-mode))
  (add-to-list 'pretty-printers (cons x 'clang-pretty-print-buffer)))


;; =====
;; ctags
;; =====


(defun create-tags-file ()
  (interactive)
  (let ((ctags (or (executable-find "ctags")
                   (error "Unable to find ctags executable in exec-path")))
        (default-directory (read-directory-name "Create ctags file at: ")))
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
      (let ((p (point)))
        (shell-command-on-region (point-min)
                                 (point-max)
                                 (format "%s --format -" xmllint)
                                 (current-buffer)
                                 t)
        (reindent-region (point-min)
                         (point-max))
        (goto-char p)))))


(add-to-list 'pretty-printers '(sgml-mode . xml-pretty-print-buffer))


;; ====
;; JSON
;; ====


(add-to-list 'pretty-printers '(js-json-mode . json-pretty-print-buffer))


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


(defun babashka ()
  (interactive)
  (cider-jack-in-universal 3))


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


(advice-add 'powershell
            :around
            (lambda (f &rest args)
              (let ((*comint-histfile-id* "powershell"))
                (prog1 (apply f args)
                  (set-buffer-process-coding-system 'cp866-dos 'cp866-dos)))))


;; ====
;; Rust
;; ====


(add-to-list 'pretty-printers '(rust-mode . rust-format-buffer))


(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'eglot-ensure))


;; ===============================
;; Serving directories with Python
;; ===============================


(defun serve-directory ()
  (interactive)
  (let* ((default-directory (read-directory-name "Serve directory: "))
         (socket (read-string "Address: " "0.0.0.0:5555"))
         (command (apply 'format "python -m http.server -b %s %s" (split-string socket ":")))
         (buffer (format "*python-server:%s*" socket)))
    (async-shell-command command buffer)
    (message "Serving %s at %s"
             (propertize default-directory 'face 'bold-italic)
             (propertize socket 'face 'bold))))


;; ================================
;; Access eng-rus dictionary online
;; ================================


(defun translate-en-ru-online (&optional query)
  "Translate from english to russian or vice versa (depending on query)"
  (interactive)
  (let* ((default-directory "~")
         (query (or query (read-string "Translate: " (word-at-point))))
         (en-ru `((command . ,(concat "bash -c \"curl -sL -A 'Mozilla/1.0' '%s"
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
         (translation (string-trim (shell-command-to-string command))))
    (if (zerop (length translation))
        (message "Can't find translation for '%s'" query)
      (message "%s =>\n%s"
               (propertize query 'face 'font-lock-constant-face)
               translation))))


;; ==========================
;; capture videos with ffmpeg
;; ==========================


(defun capture-video ()
  (interactive)
  (let ((ffmpeg (or (executable-find "ffmpeg")
                    (error "Unable to find ffmpeg executable in exec-path")))
        (buffer-name "*video-capture*"))
    (if (get-buffer-process buffer-name)
        (with-current-buffer buffer-name
          (message "Captured: %s" (propertize capture-file-name 'face 'font-lock-constant-face))
          (kill-new capture-file-name)
          (kill-buffer))
      (let* ((capture-file-name (read-file-name "Capture video to file: "))
             (default-directory (file-name-directory capture-file-name)))
        (async-shell-command
         (format "%s -y -f gdigrab -i desktop -framerate 30 -pix_fmt yuv420p %s"
                 ffmpeg
                 (car (last (file-name-split capture-file-name))))
         buffer-name)
        (with-current-buffer buffer-name
          (setq-local capture-file-name capture-file-name))
        (message "Capturing video to file: %s"
                 (propertize capture-file-name 'face 'font-lock-constant-face))))))


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
    (setq eww-download-directory (expand-file-name "Downloads" (getenv "USERPROFILE")))))


(with-eval-after-load 'shr
  ;; Use monospaced fonts by default
  (setq shr-use-fonts nil)
  (setq shr-inhibit-images t))


;; GUI browser


(setq search-engines
      '(("wb". "https://www.wildberries.ru/catalog/0/search.aspx?search=%s")))


(defun browse-url-or-search (query)
  (interactive (list (read-string "URL/search query: "
                                  (when (use-region-p)
                                    (buffer-substring (region-beginning)
                                                      (region-end)))
                                  'browser-query-history)))
  (let ((browse-url-browser-function (if (display-graphic-p)
                                         'browse-url-default-browser
                                       'eww-browse-url))
        (ddg (concat (if (display-graphic-p)
                         "https://duckduckgo.com"
                       "https://html.duckduckgo.com/html/")
                     "?q=%s")))
    (cond ((string-match-p "^[a-zA-Z0-9]+://" query)
           (browse-url query))
          ((string-prefix-p "!" query)
           (let* ((engine (cdr (assoc query search-engines
                                      (lambda (a b) (string-prefix-p (format "!%s " a) b)))))
                  (query (if engine
                             (replace-regexp-in-string "^!.+? " "" query)
                           query)))
             (browse-url (format (or engine ddg) query))))
          (t (browse-url (format ddg query))))))


(require 'ffap)


(setq ffap-url-fetcher
      (lambda (x)
        (browse-url-or-search x)
        (add-to-history 'browser-query-history x)))


;; =======================
;; youtube-dlp integration
;; =======================


(defun youtube-download (link)
  (interactive (list (read-string
                      (format "Download %s from YouTube: "
                              (if current-prefix-arg "audio" "video")))))
  (let* ((default-directory (if system-type-is-windows
                                (expand-file-name "Downloads" (getenv "USERPROFILE"))
                              "~"))
         (yt (executable-find "yt-dlp"))
         (command-download-video
          (format "%s -f mp4 '%s'" yt link))
         (command-download-audio
          (format "%s -x --audio-quality 0 --audio-format mp3 '%s'" yt link))
         (command (if current-prefix-arg
                      command-download-audio
                    command-download-video)))
    (async-shell-command command "*youtube-download*")))


;; ===========
;; epub reader
;; ===========


(require 'nov)


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(setq nov-variable-pitch t
      nov-text-width 70
      nov-font "Droid Sans")


(defun nov-scroll-up-1 ()
  (interactive)
  (scroll-up-command 1))


(defun nov-scroll-down-1 ()
  (interactive)
  (scroll-down-command 1))


(bind-keys '("j" nov-scroll-up-1
             "k" nov-scroll-down-1
             "h" nov-previous-document
             "l" nov-next-document
             "p" nov-history-back
             "n" nov-history-forward
             "<down>" nov-scroll-up-1
             "<up>" nov-scroll-down-1
             "<left>" nov-previous-document
             "<right>" nov-next-document)
           nov-mode-map)


(defun nov-font-setup ()
  (face-remap-add-relative
   'variable-pitch
   :family nov-font
   :height 1.0))


(add-hook 'nov-mode-hook 'nov-font-setup)


(defun nov-set-font (font)
  (interactive
   (list (completing-read
          "Font: "
          (cl-remove-duplicates (font-family-list) :test #'equal))))
  (setq nov-font font)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'nov-mode)
        (nov-font-setup)))))


;; ===========================
;; load site-specific settings
;; ===========================


(let ((site-file (expand-file-name "site.el" user-emacs-directory)))
  (if (file-exists-p site-file)
      (load-file site-file)
    (with-temp-buffer (write-file site-file))))
