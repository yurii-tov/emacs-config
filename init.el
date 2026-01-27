;; ======
;; System
;; ======


;; Packages


(require 'package)


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))


(when-let ((packages (cl-remove-if #'package-installed-p
                                   '(ido-grid-mode
                                     groovy-mode powershell doric-themes
                                     company yasnippet ligature diredfl
                                     multiple-cursors spacious-padding
                                     rust-mode ripgrep wgrep gptel nov
                                     slime slime-company expand-region
                                     ef-themes htmlize markdown-mode))))
  (package-refresh-contents)
  (mapc #'package-install packages))


;; Site-specific config


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(defun load-site-settings ()
  (load custom-file t)
  (load (expand-file-name "site.el" user-emacs-directory) t))


(add-hook 'emacs-startup-hook 'load-site-settings)


;; MSYS2


(when (eq system-type 'windows-nt)
  (let ((msys "C:/tools/msys64")
        (gpg (executable-find "gpg.exe"))
        (gpg-path (cl-find-if (lambda (x) (string-match "Gpg4win" x)) exec-path))
        (fail-message "Unable to integrate MSYS:"))
    (cond ((not (file-exists-p msys))
           (warn "%s msys2 distribution not found (Expected location is %s)"
                 fail-message msys))
          ((not (and gpg (string-suffix-p "GnuPG/bin/gpg.exe" gpg) gpg-path))
           (warn "%s Gpg4Win not found%s"
                 fail-message
                 (if gpg (format " (found only %s in exec-path)" gpg) "")))
          (t (setenv "PATH"
                     (format "%1$s\\mingw64\\bin;%1$s\\usr\\bin;%s"
                             (replace-regexp-in-string "/" "\\\\" msys)
                             (getenv "PATH")))
             (setenv "LC_ALL" "en_GB.UTF-8")
             (add-to-list 'exec-path (format "%s/usr/bin" msys))
             (add-to-list 'exec-path (format "%s/mingw64/bin" msys))
             (setq exec-path (cons gpg-path (cl-remove gpg-path exec-path :test #'equal))
                   shell-file-name "bash")))))


;; Termux


(when (and (eq system-type 'android)
           (getenv "TERMUX_VERSION"))
  (unless small-temporary-file-directory
    (customize-set-variable
     'small-temporary-file-directory
     "/data/data/com.termux/cache/emacs/")
    (make-directory small-temporary-file-directory t)))


;; =============
;; Text encoding
;; =============


(reset-language-environment)


(when (eq system-type 'windows-nt)
  (set-coding-system-priority 'cp1251-dos)
  (dolist (x '(compilation-start shell-command))
    (advice-add x :around
                (lambda (f &rest args)
                  "Fix args encoding"
                  (let ((coding-system-for-write 'cp1251-unix))
                    (apply f args))))))


(prefer-coding-system 'utf-8-unix)


;; ===========
;; Keybindings
;; ===========


;; Search


(define-keymap :keymap search-map
  "f" 'find-dired
  "g" 'rgrep
  "l" 'gptel-chat
  "s" 'browse-url-or-search
  "d" 'camd
  "t" 'translate-en-ru)


;; Text editing


(define-keymap :prefix 'text-edit-map
  "c" 'toggle-char-case
  "i" 'insert-char
  "p" 'insert-path
  "o" 'emoji-insert
  "x" 'reverse-region
  "s" 'sort-lines
  "d" 'shuffle-lines
  "f" 'insert-file
  "k" 'replace-string
  "M-k" 'replace-regexp
  "l" 'downcase-dwim
  "h" 'upcase-dwim
  "j" 'join-lines
  "b" 'break-line
  "m" 'keep-lines
  "n" 'flush-lines
  "q" 'fill-paragraph
  "w" 'fill-region-justify
  "u" 'delete-duplicate-lines
  "a" 'insert-fortune
  "M-c" 'duplicate-dwim
  "SPC" 'whitespace-mode)


;; Diff


(define-keymap :prefix 'diff-map
  "f" 'diff
  "b" 'diff-buffers
  "d" 'diff-current-buffer)


;; Project


(define-keymap :keymap project-prefix-map
  "/" 'project-switch-project
  "SPC" 'project-dired
  "a" 'project-async-shell-command)


;; Help


(define-keymap :keymap help-map
  "C-h" 'describe-symbol
  "h" 'describe-symbol
  "r" 'man
  "k" 'describe-keymap
  "s" 'yas-describe-tables
  "f" 'list-faces-display
  "v" 'company-diag)


;; Global


(define-keymap :keymap global-map
  "<f2>" 'kmacro-keymap
  "C-v" 'scroll-up-3
  "M-v" 'scroll-down-3
  "M-c" 'text-edit-map
  "M-o" 'other-window
  "C-1" 'delete-other-windows
  "C-2" 'split-window-below
  "C-3" 'split-window-right
  "C-0" 'delete-window
  "M-k" 'kill-line-to-indentation
  "M-=" 'count-words
  "M-q" 'hippie-expand
  "M-1" 'shell-command
  "M-!" 'asc-at-directory
  "M-2" 'enclose-text-quotes
  "M-3" 'enclose-text-asterisks
  "M-4" 'enclose-text-apostrophes
  "M-9" 'enclose-text-parenthesis
  "M-(" 'enclose-text-angle
  "M-0" 'enclose-text-square
  "M-)" 'enclose-text-curly
  "M-i" 'format-buffer
  "M-u" 'force-revert-buffer
  "M-j" 'switch-to-buffer
  "M-`" 'shell
  "M-g" 'goto-line
  "M-/" project-prefix-map
  "M-l" 'move-line-up
  "M-M" 'mc/mark-all-dwim
  "M-N" 'mc/mark-next-like-this
  "M-P" 'mc/mark-previous-like-this
  "C-<down-mouse-1>" nil
  "C-<mouse-1>" 'mc/add-cursor-on-click
  "C-M-l" 'move-line-down
  "C-=" 'text-scale-increase
  "C-M-=" 'text-scale-decrease
  "C-+" 'text-scale-reset
  "C-x ," 'compile
  "C-x ." 'recompile
  "C-x =" 'hl-line-mode
  "C-x o" 'open-in-external-program
  "C-x i" 'what-cursor-position
  "C-x d" 'diff-map
  "C-x b" 'bookmark-set
  "C-x B" 'bookmark-delete
  "C-x j" 'bookmark-jump
  "C-x u" 'reopen-with-sudo
  "C-x l" 'eglot
  "C-x C-b" 'ibuffer
  "C-x C-k" 'kill-buffer-and-window
  "C-x C-=" 'display-line-numbers-mode
  "C-x C-l" 'gptel-menu
  "C-c j" 'cider-start-map
  "C-c k" 'sql-connect
  "C-c i" 'ielm
  "C-c s" 'ssh
  "C-c d" 'serve-directory
  "C-c v" 'capture-video
  "C-c h" 'hexl-mode
  "C-c c" 'org-capture
  "C-c a" 'org-agenda
  "C-c o" 'org-push
  "C-c O" 'org-pull
  "C-c w" 'watch-file)


;; Conflicts


(keymap-unset prog-mode-map "M-q")


(with-eval-after-load 'diff-mode
  (keymap-unset diff-mode-map "M-o"))


(with-eval-after-load 'sgml-mode
  (keymap-unset html-mode-map "M-o"))


;; =============
;; Look and feel
;; =============


;; Ligatures


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


(global-ligature-mode t)


;; Current line indicator


(global-hl-line-mode)


(defun disable-hl-line-mode ()
  (hl-line-mode 'toggle))


(dolist (x '(comint-mode-hook
             vc-git-log-edit-mode-hook
             cider-repl-mode-hook
             slime-repl-mode-hook))
  (add-hook x 'disable-hl-line-mode))


;; Line numbers


(dolist (x '(prog-mode-hook
             conf-mode-hook
             sgml-mode-hook
             tabulated-list-mode-hook))
  (add-hook x 'display-line-numbers-mode))


;; Fonts


(when window-system
  (dolist (x '((default . ("Cascadia Code" "Consolas" "Ubuntu Mono"))
               (fixed-pitch . ("Consolas" "Ubuntu Mono"))))
    (if-let* ((face (car x))
              (fonts (cdr x))
              (font (cl-find-if (lambda (x)
                                  (member x (font-family-list)))
                                fonts)))
        (set-face-attribute face nil :font (format "%s-13" font))
      (message "Can't find any of %s fonts for '%s' face"
               (prin1-to-string fonts) face))))


(when (and (eq system-type 'windows-nt)
           (member "Segoe UI Emoji" (font-family-list)))
  (set-fontset-font t 'unicode
                    (font-spec :family "Segoe UI Emoji")
                    nil 'append))


;; Colors


(advice-add 'load-theme
            :around
            (lambda (f &rest args)
              (interactive (thread-last
                             (custom-available-themes)
                             (mapcar #'symbol-name)
                             (completing-read "Load custom theme: ")
                             intern
                             list))
              (dolist (theme custom-enabled-themes)
                (disable-theme theme))
              (apply f args)))


(when window-system
  (load-theme 'modus-vivendi))


;; Mode line


(defun mode-line-selection-stats ()
  (let* ((start (region-beginning))
         (end (region-end))
         (chars (- end start)))
    (when (> chars 0)
      (propertize (format " Sel: %d|%d" chars (count-lines start end))
                  'face 'fixed-pitch))))


(setq-default mode-line-format
              `(" "
                (:eval (if (memq buffer-file-coding-system
                                 '(utf-8-unix
                                   prefer-utf-8-unix
                                   undecided-unix
                                   mule-utf-8-unix
                                   no-conversion))
                           ""
                         (format "%s " (propertize
                                        (symbol-name buffer-file-coding-system)
                                        'face '(:slant italic)))))
                ,(if (and (not window-system) (eq system-type 'windows-nt))
                     'mode-line-modified
                   '(:eval (let ((ro buffer-read-only)
                                 (m (and (buffer-file-name) (buffer-modified-p))))
                             (cond ((and m ro) "🔏 ")
                                   (ro "🔒 ")
                                   (m "✒ ")
                                   (t "")))))
                ,(unless (and (not window-system) (eq system-type 'windows-nt))
                   '(:eval (if (get-buffer-process (current-buffer))
                               (propertize "• " 'face 'success)
                             "")))
                ,(propertize "%b" 'face 'mode-line-buffer-id)
                ,(propertize " %l:%C" 'face 'fixed-pitch)
                mode-line-format-right-align
                (mark-active (:eval (mode-line-selection-stats)))
                (current-input-method-title
                 (:eval (format " %s" (propertize
                                       current-input-method-title
                                       'face 'mode-line-emphasis))))
                (multiple-cursors-mode mc/mode-line)
                (flymake-mode (" " (:eval (flymake--mode-line-counters))))
                (vc-mode vc-mode)
                " "))


;; Text wrapping


(dolist (x '(man-common-hook
             flymake-diagnostics-buffer-mode-hook
             flymake-project-diagnostics-mode-hook))
  (add-hook x 'visual-line-mode))


;; Hints about complex shortcuts


(which-key-mode)


;; Fewer distractions


(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      ring-bell-function 'ignore
      use-short-answers t
      kill-buffer-query-functions nil
      disabled-command-function nil
      confirm-kill-processes nil)


;; Clean UI


(spacious-padding-mode 1)


(when fringe-indicator-alist
  (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil)))


(unless window-system
  (set-display-table-slot standard-display-table 'wrap ?\ ))


(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))


(blink-cursor-mode 0)


(tool-bar-mode -1)


(menu-bar-mode -1)


;; Fail-safe shutdown


(setq confirm-kill-emacs 'y-or-n-p)


;; Scrolling


(setq scroll-conservatively 100
      scroll-preserve-screen-position t)


(defun scroll-up-3 ()
  (interactive)
  (scroll-up-line 3))


(defun scroll-down-3 ()
  (interactive)
  (scroll-down-line 3))


(when (eq system-type 'android)
  (define-keymap :keymap global-map
    "<wheel-down>" 'scroll-up-line
    "<wheel-up>" 'scroll-down-line))


;; Scaling


(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))


;; Monday-based calendar


(setq calendar-week-start-day 1)


;; Scratch buffer


(defun scratch-buffer-setup ()
  (with-current-buffer "*scratch*"
    (erase-buffer)
    (insert (replace-regexp-in-string "\n" "" (emacs-version)))
    (newline 3)
    (insert-fortune)
    (comment-region (point-min) (point-max))
    (newline 3)))


(add-hook 'emacs-startup-hook 'scratch-buffer-setup)


;; ==========
;; Minibuffer
;; ==========


(setq max-mini-window-height 12)


;; Completion


(fido-vertical-mode)


(define-keymap :keymap icomplete-minibuffer-map
  "C-j" 'icomplete-fido-exit
  "M-j" 'icomplete-force-complete
  "SPC" 'self-insert-command
  "?" 'self-insert-command)


(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq-local completion-styles '(substring flex)
                        completion-category-defaults nil)))


(advice-add 'completion-flex--make-flex-pattern
            :around
            (lambda (f &rest args)
              "Don't use flex on long patterns in minibuffer"
              (if (or (not (minibufferp))
                      (<= (length (cadar args)) 16))
                  (apply f args)
                (car args))))


(advice-add 'read-string
            :around
            (lambda (f &rest args)
              "Use completing-read"
              (let* ((prompt (car args))
                     (initial-input (cadr args))
                     (history-symbol (or (car-safe (caddr args))
                                         (caddr args)))
                     (history (and (not (eq history-symbol t))
                                   (boundp history-symbol)
                                   (symbol-value history-symbol))))
                (if (and history
                         (not (memq history-symbol
                                    '(string-rectangle-history
                                      junk-hist
                                      org-read-date-history
                                      transient--history))))
                    (completing-read prompt history
                                     nil nil initial-input history-symbol)
                  (apply f args)))))


;; History


(require 'savehist)


(setq history-delete-duplicates t)


(add-hook 'emacs-startup-hook 'savehist-mode)


;; =====
;; TRAMP
;; =====


(setq tramp-remote-path '(tramp-default-remote-path
                          "/data/data/com.termux/files/usr/bin"))


;; =======
;; Buffers
;; =======


(require 'uniquify)


(setq uniquify-buffer-name-style 'forward
      confirm-nonexistent-file-or-buffer nil)


(advice-add 'read-buffer-to-switch
            :around
            (lambda (f &rest args)
              "Add 'Create new buffer' shortcut"
              (minibuffer-with-setup-hook
                  (:append (lambda ()
                             (use-local-map
                              (define-keymap :parent (current-local-map)
                                "M-j"
                                (lambda ()
                                  (interactive)
                                  (let ((b (generate-new-buffer "*scratch*")))
                                    (with-current-buffer b
                                      (org-mode))
                                    (insert (buffer-name b)))
                                  (exit-minibuffer))))))
                (apply f args))))


(advice-add 'read-buffer-to-switch
            :around
            (lambda (f &rest args)
              "Display working directories"
              (let ((completion-extra-properties
                     `(:annotation-function
                       (lambda (x)
                         (with-current-buffer x
                           (concat " " (string-truncate-left
                                        (abbreviate-file-name
                                         default-directory)
                                        (- (- (window-width) 3)
                                           (length x)))))))))
                (apply f args))))


(advice-add 'kill-buffer-and-window
            :around
            (lambda (f &rest args)
              "Fix 'single window' broken scenario"
              (if (> (length (window-list)) 1)
                  (apply f args)
                (kill-buffer))))


;; =======
;; Ibuffer
;; =======


(setq ibuffer-expert t
      ibuffer-default-sorting-mode 'alphabetic
      ibuffer-show-empty-filter-groups nil
      ibuffer-formats '((mark modified read-only locked " "
                              (name 16 -1)
                              " " filename))
      ibuffer-saved-filter-groups
      '(("default"
         ("System" (or (mode . messages-buffer-mode)
                       (and (mode . lisp-interaction-mode)
                            (name . "\\*scratch"))))
         ("💬 REPL" (or (and (or (and (derived-mode . comint-mode)
                                      (not (mode . shell-command-mode)))
                                 (mode . cider-repl-mode))
                             (predicate . (get-buffer-process (current-buffer))))
                        (and (mode . slime-repl-mode)
                             (predicate . (slime-connected-p)))))
         ("⚗️ Build" (and (mode . compilation-mode)
                         (predicate . (get-buffer-process (current-buffer)))))
         ("🦄 Org" (mode . org-mode))
         ("💽 Program" (derived-mode . prog-mode))
         ("📄 Text" (and (or (derived-mode . text-mode)
                             (mode . fundamental-mode)
                             (derived-mode . conf-mode))
                         (not (name . "\\*"))))
         ("📖 Docs" (or (mode . Man-mode)
                        (mode . Info-mode)))
         ("📚 Book" (mode . nov-mode))
         ("🌐 EWW" (or (mode . eww-buffers-mode)
                       (mode . eww-mode)
                       (mode . eww-history-mode)
                       (mode . eww-bookmark-mode)))
         ("📁 Directory" (mode . dired-mode))
         ("🧠 LSP" (name . "\\*EGLOT"))
         ("🧵 Process" (predicate . (get-buffer-process (current-buffer))))
         ("📦 Misc" (predicate . t)))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))


(advice-add 'ibuffer-filter-disable
            :around
            (lambda (f &rest args)
              "Switch to recent filter unless filtering is active"
              (if ibuffer-filtering-qualifiers
                  (progn (setq-local last-filter ibuffer-filtering-qualifiers)
                         (apply f args))
                (when (boundp 'last-filter)
                  (setq ibuffer-filtering-qualifiers last-filter)
                  (ibuffer-update nil)))))


(with-eval-after-load 'ibuffer
  (define-keymap :keymap ibuffer-mode-map
    "M-j" nil "M-o" nil
    "j" 'ibuffer-jump-to-filter-group))


;; =====
;; Files
;; =====


(global-auto-revert-mode t)


(setq auto-save-default nil
      make-backup-files nil
      auto-save-list-file-name nil
      auto-revert-verbose nil
      revert-without-query '(".*")
      auto-revert-use-notify (not (eq system-type 'windows-nt)))


(defun force-revert-buffer ()
  (interactive)
  (message "Force reverting buffer '%s'..." (buffer-name))
  (yas-exit-all-snippets)
  (revert-buffer nil t t))


(defun open-in-external-program (file-name)
  (interactive "fOpen in external program: ")
  (let ((open-file
         (cond ((eq system-type 'windows-nt)
                (lambda (f) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" f t t))))
               ((eq system-type 'darwin)
                (lambda (f) (shell-command (concat "open " (shell-quote-argument f)))))
               ((eq system-type 'gnu/linux)
                (lambda (f) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" f)))))))
    (funcall open-file file-name)))


(defun watch-file (file)
  (interactive "fWatch file: ")
  (let ((default-directory (file-name-directory file))
        (current-prefix-arg t))
    (async-shell-command (concat "tail -f " (file-relative-name file)))))


(defun reopen-with-sudo ()
  (interactive)
  (let ((file (or (buffer-file-name) default-directory)))
    (kill-buffer)
    (find-file
     (if-let* ((host (file-remote-p file 'host)))
         (concat (replace-regexp-in-string host
                                           (concat host "|sudo:")
                                           (file-remote-p file))
                 (file-remote-p file 'localname))
       (concat "/sudo::" file)))))


(defun diff-current-buffer ()
  "Invoke `diff-buffer-with-file' for current buffer"
  (interactive)
  (diff-buffer-with-file))


;; ===
;; IDO
;; ===


(ido-mode 'files)


(ido-grid-mode)


(setq read-file-name-function #'ido-read-file-name
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-grid-mode-prefix ""
      ido-grid-mode-exact-match-prefix ""
      ido-grid-mode-first-line '(" " ido-grid-mode-count)
      ido-grid-mode-keys '(up down left right)
      ido-auto-merge-work-directories-length -1
      ido-grid-mode-max-rows 10
      ido-grid-mode-min-rows 5
      ido-report-no-match nil)


(advice-add 'ido-read-file-name
            :filter-return
            (lambda (file-name)
              "Update history"
              (prog1 file-name
                (unless (file-directory-p file-name)
                  (ido-record-work-directory
                   (file-name-directory file-name))
                  (add-to-history 'file-name-history file-name)))))


(advice-add 'ido-complete
            :after
            (lambda ()
              "Auto-switch to completions buffer"
              (when (cdr ido-matches)
                (ido-switch-to-completions))))


(advice-add 'ido-grid-mode-ido-setup
            :around
            (lambda (f &rest args)
              "Prevent global settings tampering"
              (let ((h max-mini-window-height)
                    (r resize-mini-windows))
                (apply f args)
                (setq max-mini-window-height h
                      resize-mini-windows r))))


(advice-add 'ido-grid-mode-count
            :override
            (lambda ()
              "Count items currently matching the pattern"
              (propertize (format "(%s)" (length ido-matches))
                          'face 'font-lock-comment-face)))


;; Colors


(custom-set-faces
 '(ido-first-match ((t (:inherit icomplete-selected-match))))
 '(ido-only-match ((t (:inherit icomplete-selected-match))))
 '(ido-grid-mode-match ((t (:inherit completions-common-part))))
 '(ido-subdir ((t (:inherit default)))))


;; Keybindings


(define-keymap :keymap ido-file-dir-completion-map
  "C-b" nil "M-f" nil "M-n" nil "M-<up>" nil
  "C-f" nil "M-k" nil "M-p" nil "M-<down>" nil
  "C-n" 'ido-grid-mode-next
  "C-p" 'ido-grid-mode-previous
  "SPC" 'ido-recent
  "M-SPC" 'ido-search-subdirs)


;; Advanced commands


(defun ido-search-subdirs ()
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (icomplete-mode t)
         (exclusions (mapcar (lambda (dir) (concat dir "/"))
                             vc-directory-exclusion-list))
         (files (mapcar (lambda  (x) (file-relative-name
                                      x ido-current-directory))
                        (project--files-in-directory
                         ido-current-directory
                         exclusions)))
         (file (expand-file-name
                (completing-read
                 (format "Find file in %s: "
                         (abbreviate-file-name ido-current-directory))
                 files nil nil ido-text)
                ido-current-directory)))
    (setq ido-matches (list (file-name-nondirectory file)))
    (when-let ((d (file-name-directory file)))
      (setq ido-current-directory (expand-file-name d ido-current-directory)))
    (when (file-directory-p file)
      (setq ido-exit 'refresh))
    (exit-minibuffer)))


(defun ido-recent ()
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (icomplete-mode t)
         (disconnected-p (lambda (x)
                           (and (file-remote-p x)
                                (not (file-remote-p x nil t)))))
         (history (cl-remove-if-not disconnected-p file-name-history))
         (files (thread-last
                  ido-work-directory-list
                  (cl-remove-if (lambda (x)
                                  (or (funcall disconnected-p x)
                                      (not (file-exists-p x))
                                      (ido-nonreadable-directory-p x))))
                  (mapcan (lambda (x)
                            (thread-last (if (eq ido-cur-item 'dir) "*/" "*")
                                         (concat x)
                                         file-expand-wildcards
                                         (cons x))))
                  (nconc (if (eq ido-cur-item 'dir)
                             (cl-delete-if-not
                              (lambda (x) (string-suffix-p "/" x)) history)
                           history))))
         (file (completing-read "Find recent: "
                                files nil nil ido-text
                                (if (eq ido-cur-item 'dir)
                                    'ido-work-directory-list
                                  'file-name-history))))
    (setq ido-current-directory (file-name-directory file)
          ido-matches (list (file-name-nondirectory file)))
    (when (file-directory-p file)
      (setq ido-current-directory (file-name-as-directory file)
            ido-exit 'refresh))
    (exit-minibuffer)))


;; =====
;; Dired
;; =====


(add-hook 'dired-mode-hook
          (lambda ()
            (diredfl-mode)
            (auto-revert-mode)
            (setq-local ido-use-filename-at-point nil)))


(require 'ls-lisp)


(setq ls-lisp-use-insert-directory-program nil
      ls-lisp-format-time-list
      '("%Y-%m-%d %H:%M"
        "%Y-%m-%d %H:%M")
      ls-lisp-use-localized-time-format t
      ls-lisp-dirs-first t
      dired-listing-switches "-alh"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      wdired-allow-to-change-permissions t)


(advice-add 'ls-lisp-set-options
            :after
            (lambda ()
              "Force ls-lisp-dirs-first = t"
              (setq ls-lisp-dirs-first t)))


(advice-add 'dired-copy-filename-as-kill
            :override
            (lambda (&rest _)
              (when-let* ((xs (dired-get-marked-files))
                          (n (length xs)))
                (kill-new (string-join xs "\n"))
                (message (if (= n 1)
                             (format "Copied to clipboard: %s" (car xs))
                           (format "Copied %d filenames to clipboard" n))))))


;; Keybindings


(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "j" 'dired
    "o" 'dired-display-file
    "h" 'dired-mark-files-regexp
    "l" 'dired-up-directory
    "a" 'dired-do-compress-to
    "A" 'dired-extract
    "z" 'dired-do-compress
    "f" 'dired-flatten-directory
    "s" 'dired-calculate-size
    "S" 'dired-calculate-size-tree
    "c" 'dired-do-copy
    "r" 'dired-do-rename
    "k" 'dired-do-delete
    "e" 'dired-toggle-read-only
    "1" 'dired-do-chmod
    "2" 'dired-do-chown
    "3" 'dired-do-touch
    "y" 'dired-do-symlink
    "TAB" 'dired-sort-toggle-or-edit
    "SPC" 'scroll-up-command
    "S-SPC" 'scroll-down-command
    "M-s" nil))


;; Archiver


(with-eval-after-load 'dired-aux
  (dolist (x '(("\\.[^./]+$" . "zip %o -r --filesync %i")
               (".*" . "tar -cf - %i | gzip -c9 > %o.tar.gz")))
    (add-to-list 'dired-compress-files-alist x t)))


(defun dired-extract (dir)
  (interactive "DExtract to: ")
  (let* ((dir (or (file-remote-p dir 'localname) dir))
         (dired-no-confirm t)
         (tar-command (format "tar -xz -C %s < %%i"
                              (if (eq system-type 'windows-nt)
                                  "$(cygpath -u '%o')" "%o")))
         (dired-compress-file-suffixes
          (mapcar (lambda (x)
                    (if-let ((c (nth 2 x)))
                        (append (seq-take x 2)
                                (list (string-replace "%o" dir c)))
                      x))
                  (append (cons (list "\\.tar\\.gz\\'" "" tar-command)
                                dired-compress-file-suffixes)
                          '(("\\.rar\\'" "" "7z x -aoa -o%o %i")
                            (".*" "" "unzip -o -d %o %i"))))))
    (dired-do-compress)))


;; Extra commands


(defun dired-flatten-directory ()
  "Run find program on selected directory"
  (interactive)
  (let ((directory (file-truename (car (dired-get-marked-files)))))
    (if (file-directory-p directory)
        (progn (kill-buffer)
               (find-dired directory ""))
      (message "This command works only on directories"))))


(defun dired-calculate-size (&optional tree-p)
  (interactive)
  (let* ((files (mapcar #'file-relative-name
                        (dired-get-marked-files)))
         (args (mapcar (lambda (x) (format "'%s'" x)) files)))
    (message "Calculating size of %s..."
             (string-join (mapcar (lambda (x)
                                    (propertize x 'face 'success))
                                  files)
                          ", "))
    (shell-command
     (if tree-p
         (format "tree --du -h %s"
                 (string-join args  " "))
       (format "du -hs%s %s"
               (if (> (length args) 1) "c" "")
               (string-join args  " "))))))


(defun dired-calculate-size-tree ()
  (interactive)
  (dired-calculate-size t))


;; History


(advice-add 'dired--find-possibly-alternative-file
            :after
            (lambda (&rest args)
              "When open file, update ido-work-directory-list"
              (let ((file (car args)))
                (unless (file-directory-p file)
                  (ido-record-work-directory
                   (file-name-directory file))))))


;; ====
;; Find
;; ====


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun find-dired-setup-buffer (f &rest args)
  (let* ((buffer-name (thread-first (car args)
                                    directory-file-name
                                    file-name-base))
         (buffer (get-buffer buffer-name)))
    (when (and buffer
               (file-equal-p (with-current-buffer buffer
                               default-directory)
                             (car args))
               (eq (with-current-buffer buffer major-mode)
                   'dired-mode))
      (kill-buffer buffer))
    (apply f args)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (kill-buffer (current-buffer))
            (apply #'find-dired ',args)))
    (rename-buffer buffer-name t)))


(advice-add 'find-dired :around #'find-dired-setup-buffer)


(advice-add 'find-dired
            :around
            (lambda (f &rest args)
              "Search by partial file name, unless explicit switches provided"
              (let ((query (cadr args)))
                (unless (string-prefix-p "-" query)
                  (setf (cadr args)
                        (format "-iname '*%s*'" query)))
                (apply f args))))


(advice-add 'find-dired
            :after
            (lambda (&rest _)
              "Blank prompt"
              (setq find-args "")))


(advice-add 'find-dired
            :around
            (lambda (f &rest xs)
              "More consistent querying of user inputs"
              (interactive)
              (let* ((query (if (called-interactively-p)
                                (read-string "Search for files: ")
                              (cadr xs)))
                     (dir (if (called-interactively-p)
                              (read-directory-name
                               (format "Search for %s at: "
                                       (propertize (if (string-empty-p query)
                                                       "*" query)
                                                   'face 'success)))
                            (car xs))))
                (funcall f dir query))))


;; =======
;; Ripgrep
;; =======


(setq ripgrep (executable-find "rg")
      ripgrep-arguments '("-uu"))


(defun project-ripgrep (regexp)
  (interactive (list (project--read-regexp)))
  (let ((ripgrep-arguments nil))
    (ripgrep-regexp regexp (project-root (project-current t)))))


(when ripgrep
  (keymap-set search-map "g" 'ripgrep-regexp)
  (advice-add 'project-find-regexp :override #'project-ripgrep))


(defun ripgrep-dired ()
  "Collect the files into Dired buffer"
  (interactive)
  (let (result)
    (save-excursion
      (goto-line 4)
      (end-of-line)
      (while-let ((p (re-search-forward "^[^:]+:[0-9]" nil t)))
        (cl-pushnew (buffer-substring-no-properties
                     (line-beginning-position)
                     (- p 2))
                    result
                    :test #'equal)))
    (pop result)
    (if result
        (dired-other-window
         (cons compilation-directory
               (nreverse (mapcar #'file-relative-name result))))
      (message "File list is empty"))))


(defun ripgrep-setup ()
  (setq-local compilation-scroll-output nil))


(with-eval-after-load 'ripgrep
  (define-keymap :keymap ripgrep-search-mode-map
    "TAB" 'compilation-next-error
    "<backtab>" 'compilation-previous-error
    "d" 'ripgrep-dired
    "n" 'next-error-no-select
    "p" 'previous-error-no-select
    "o" 'compilation-display-error
    "e" 'wgrep-change-to-wgrep-mode)
  (add-hook 'ripgrep-search-mode-hook
            'ripgrep-setup)
  (setq wgrep-auto-save-buffer t))


;; ===========
;; Text editor
;; ===========


;; Input methods


(setq default-input-method 'russian-computer)


;; Read-only mode


(setq view-read-only t
      view-no-disable-on-exit t
      view-inhibit-help-message t)


(advice-add 'View-exit :override #'read-only-mode)


;; Search


(setq isearch-lazy-count t)


(define-keymap :keymap isearch-mode-map
  "M-w" 'isearch-toggle-word
  "M-," 'isearch-beginning-of-buffer
  "M-." 'isearch-end-of-buffer
  "M-SPC" 'isearch-query-replace)


(dolist (x '(regexp-search-ring search-ring))
  (add-to-list 'savehist-additional-variables x))


;; Replace


(keymap-set query-replace-map "M-SPC" 'automatic)


;; Overwriting


(delete-selection-mode t)


;; Selecting


(advice-add 'kill-ring-save
            :around
            (lambda (f &rest args)
              "Unless region is active, invoke expand-region"
              (if (use-region-p)
                  (apply f args)
                (call-interactively 'er/expand-region))))


;; Formatting


(setq-default indent-tabs-mode nil
              fill-column 80)


(setq js-indent-level 2
      css-indent-offset 2
      c-basic-offset 4)


(advice-add 'c-indent-line-or-region :override #'indent-for-tab-command)


(defun fill-region-justify (start end)
  (interactive (buffer-or-region))
  (fill-region start end 'full))


(setq format-buffer-functions
      '((js-json-mode . json-pretty-print-buffer)
        (rust-mode . rust-format-buffer)))


(advice-add 'indent-region
            :around
            (lambda (f &rest args)
              "Fix whitespace problems"
              (let ((s (car args))
                    (e (cadr args))
                    (p (point-max)))
                (whitespace-cleanup)
                (apply f s (- e (- p (point-max))) (cddr args)))))


(defun format-buffer (&optional command)
  "Formats current buffer with suitable function from `format-buffer-functions', using reindent as fallback.
Optionally, formats the buffer with COMMAND (if provided)"
  (interactive)
  (if command
      (let ((shell-file-name "sh")
            (p (point))
            (b (current-buffer))
            (s (buffer-substring-no-properties
                (point-min) (point-max))))
        (with-temp-buffer
          (insert s)
          (shell-command-on-region (point-min) (point-max) command nil t)
          (let ((pprinted (buffer-substring-no-properties
                           (point-min) (point-max))))
            (with-current-buffer b
              (unless (string-equal pprinted s)
                (erase-buffer)
                (insert pprinted)
                (goto-char p))))))
    (if-let ((f (cdr (assoc major-mode format-buffer-functions))))
        (progn (message "Formatting buffer...")
               (funcall f)
               (message
                (if (buffer-modified-p)
                    "Formatting buffer...Done"
                  "Already well-formatted")))
      (deactivate-mark)
      (indent-region (point-min) (point-max)))))


;; Inspection


(setq what-cursor-show-names t)


(advice-add 'hexl-mode
            :around
            (lambda (f &rest args)
              "When region is selected, run hexdump program on it"
              (if (use-region-p)
                  (shell-command-on-region
                   (region-beginning)
                   (region-end)
                   "hexdump -C")
                (apply f args))))


;; Duplicating


(setq duplicate-line-final-position -1)


;; Insert


(defun fortune ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "fortune.txt"
                                            user-emacs-directory))
    (goto-char (1+ (cl-random (point-max))))
    (let* ((e (search-forward-regexp "^%$" nil t))
           (e (if e (- e 2) (point-max))))
      (goto-char e)
      (let* ((s (search-backward-regexp "^%$" nil t))
             (s (if s (+ 2 s) 1)))
        (buffer-substring s e)))))


(defun insert-fortune ()
  (interactive)
  (insert (fortune)))


(defun insert-path (path)
  (interactive "fInsert path: ")
  (insert path))


;; Enclosing into parenthesis (or similar)


(defun enclose-text (b1 b2)
  (if-let* ((region (cond ((use-region-p)
                           (car (region-bounds)))
                          ((when-let* ((bounds (bounds-of-thing-at-point
                                                'sexp))
                                       ((= (point) (car bounds))))
                             bounds))
                          ((not (looking-at "[[:blank:]]\\|$\\|[\])}>\"'`;*]"))
                           (bounds-of-thing-at-point 'symbol))))
            (s (car region))
            (e (cdr region)))
      (progn (goto-char s)
             (insert b1)
             (goto-char e)
             (forward-char (length b2))
             (insert b2)
             (goto-char (1+ s)))
    (insert b1)
    (insert b2)
    (backward-char)))


(defun enclose-text-parenthesis ()
  (interactive)
  (enclose-text "(" ")"))


(defun enclose-text-square ()
  (interactive)
  (enclose-text "[" "]"))


(defun enclose-text-curly ()
  (interactive)
  (enclose-text "{" "}"))


(defun enclose-text-angle ()
  (interactive)
  (enclose-text "<" ">"))


(defun enclose-text-quotes ()
  (interactive)
  (enclose-text "\"" "\""))


(defun enclose-text-asterisks ()
  (interactive)
  (enclose-text "*" "*"))


(defun enclose-text-apostrophes ()
  (interactive)
  (enclose-text "'" "'"))


;; Multiple cursors


(setq mc/always-run-for-all t)


;; Line-based modifications


(defun buffer-or-region ()
  (if (use-region-p)
      (list (region-beginning)
            (region-end))
    (list (point-min)
          (point-max))))


(defun shuffle-lines (start end)
  (interactive (buffer-or-region))
  (message "Shuffling lines...")
  (let* ((end (min (point-max) (1+ end))))
    (shell-command-on-region start end "shuf" nil t)))


(advice-add 'sort-lines
            :around
            (lambda (f &rest args)
              "Use `buffer-or-region' when called interactively"
              (interactive (cons current-prefix-arg (buffer-or-region)))
              (apply f args)))


(dolist (x '(delete-duplicate-lines reverse-region))
  (advice-add x :around
              (lambda (f &rest args)
                "Use `buffer-or-region' when called interactively"
                (interactive (buffer-or-region))
                (apply f args))))


(dolist (x '(flush-lines keep-lines))
  (advice-add x :around
              (lambda (f &rest args)
                "Fix in various ways"
                (let* ((s (car args))
                       (args (if (string-empty-p s)
                                 (cons "^$" (cdr args))
                               args)))
                  (if (use-region-p)
                      (apply f args)
                    (save-excursion
                      (apply f (car args)
                             (point-min)
                             (point-max)
                             (cdddr args))))))))


(defun join-lines ()
  (interactive)
  (if (use-region-p)
      (let* ((separator (read-string "Join with: "))
             (text (thread-first (buffer-substring (region-beginning)
                                                   (region-end))
                                 (split-string "\n" t " *")
                                 (string-join separator))))
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
  (let* ((separator (read-string "Break with: "))
         (text (thread-first (buffer-substring
                              (line-beginning-position)
                              (line-end-position))
                             (split-string separator t)
                             (string-join "\n"))))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert text)))


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


(defun move-line-up ()
  (interactive)
  (move-line 'up))


(defun move-line-down ()
  (interactive)
  (move-line 'down))


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


;; Case altering


(defun toggle-char-case ()
  (interactive)
  (save-excursion
    (if (char-uppercase-p (following-char))
        (downcase-region (point) (1+ (point)))
      (upcase-region (point) (1+ (point))))))


;; ===================
;; Completion at point
;; ===================


(setq tab-always-indent 'complete
      completion-styles '(partial-completion flex)
      completion-auto-select t)


;; =============
;; Hippie-expand
;; =============


(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-line
        try-expand-list
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-visible
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-dabbrev-all-buffers
        try-expand-line-all-buffers
        try-expand-list-all-buffers))


;; =========
;; Yasnippet
;; =========


(setq yas-verbosity 1)


(yas-global-mode 1)


;; =======
;; Company
;; =======


(global-company-mode)


(company-tng-mode)


(advice-remove #'eglot--snippet-expansion-fn #'ignore)


(eldoc-add-command 'company-smart-complete)


(setq company-tooltip-offset-display 'lines
      company-selection-wrap-around t
      company-dabbrev-code-modes '(prog-mode)
      company-dabbrev-code-completion-styles t
      company-backends '(company-capf
                         company-dabbrev-code
                         company-files
                         company-dabbrev))


(advice-add 'completion-at-point
            :around
            (lambda (f)
              "Use Company for explicit completion"
              (if (minibufferp)
                  (funcall f)
                (company-complete-common))))


(dolist (x '(company-dabbrev-code company-dabbrev))
  (advice-add x :around (lambda (f &rest args)
                          "Prevent completion on empty prefix"
                          (let ((r (apply f args)))
                            (cl-case (car args)
                              (prefix (unless (string-empty-p (car r)) r))
                              (t r))))))


(advice-add 'company-select-next
            :around
            (lambda (f &rest args)
              "If a complete Yasnippet has been typed,
 or if sole candidate present, confirms the selection right away"
              (cond (company-selection (apply f args))
                    ((yas-expand) (company-abort))
                    ((null (cdr company-candidates))
                     (company-select-first)
                     (company-complete))
                    (t (apply f args)))))


(defun company-smart-complete ()
  "Does special action depending on context.
   - When navigating paths: proceeds to deeper level
   - When expanding a snippet: prevents any extra edits
   - When on active snippet field: skips to the next field"
  (interactive)
  (cond ((and company-selection
              (string-suffix-p "/" (nth company-selection
                                        company-candidates)))
         (company-complete-selection)
         (company-manual-begin))
        ((and company-selection
              (memq (company-call-backend 'kind (nth company-selection
                                                     company-candidates))
                    '(snippet method)))
         (company-complete))
        ((and company-selection
              (get-char-property (1- (point)) 'yas--field))
         (company-complete)
         (yas-next-field-or-maybe-expand))
        (t (when company-selection
             (company-complete))
           (call-interactively 'self-insert-command))))


(dotimes (n 10)
  (keymap-unset company-active-map (format "M-%d" n)))


(dolist (x (list company-active-map
                 company-search-map))
  (define-keymap :keymap x
    "M-p" nil "M-n" nil
    "SPC" 'company-smart-complete))


;; ==============
;; Shell commands
;; ==============


(setq shell-command-prompt-show-cwd t)


;; History completion


(advice-add 'read-shell-command
            :around
            (lambda (f &rest args)
              "Use completing-read"
              (let* ((history-arg (or (caddr args) 'shell-command-history))
                     (history-symbol (or (car-safe history-arg) history-arg))
                     (history (and (boundp history-symbol)
                                   (symbol-value history-symbol))))
                (if history
                    (completing-read
                     (car args) history nil nil (cadr args) history-symbol)
                  (apply f args)))))


;; Pipe region into the command


(advice-add 'shell-command
            :around
            (lambda (f &rest args)
              "When region is selected, pipe it into the command"
              (if (use-region-p)
                  (shell-command-on-region (region-beginning)
                                           (region-end)
                                           (car args)
                                           nil
                                           current-prefix-arg)
                (apply f args))))


;; ====================
;; Async shell commands
;; ====================


(defun asc-at-directory (command directory)
  (interactive (let ((command (read-shell-command "Async shell command: ")))
                 (list command
                       (read-directory-name
                        (format "Run `%s` at: "
                                (thread-first
                                  command
                                  (truncate-string-to-width
                                   20 nil nil t)
                                  (propertize 'face 'success)))))))
  (let* ((default-directory directory))
    (async-shell-command command)))


(add-hook 'shell-command-mode-hook 'read-only-mode)


;; Buffer


(defun asc-handle-existing (buffer-name)
  (if-let* (((get-buffer buffer-name))
            ((get-buffer-process buffer-name))
            (dir (with-current-buffer buffer-name
                   (abbreviate-file-name default-directory))))
      (if (y-or-n-p (format
                     "The process is already running at %s. Kill it?"
                     (propertize dir 'face 'completions-annotations)))
          (prog1 buffer-name
            (kill-buffer buffer-name)
            (sit-for 0.5))
        (generate-new-buffer-name buffer-name))
    buffer-name))


(advice-add 'async-shell-command
            :around
            (lambda (f &rest args)
              "Setup the buffer"
              (let* ((command (car args))
                     (buffer-name (or (cadr args)
                                      (format "*%s*"
                                              (truncate-string-to-width
                                               command 32 nil nil t)))))
                (apply f command
                       (asc-handle-existing buffer-name)
                       (cddr args)))))


;; Startup details


(advice-add 'async-shell-command
            :around
            (lambda (f &rest args)
              "Place startup details into the buffer"
              (let* ((r (apply f args))
                     (b (asc-buffer r))
                     (p (get-buffer-process b)))
                (prog1 r
                  (with-current-buffer b
                    (let ((info (format "*** `%s` at %s ***\n"
                                        (car args)
                                        (abbreviate-file-name default-directory))))
                      (goto-char 1)
                      (comint-output-filter p info)
                      (set-marker comint-last-input-end (point))
                      (highlight-regexp (regexp-quote info) 'shadow)
                      (font-lock-update)))))))


;; Process state transitions


(advice-add 'async-shell-command
            :around
            (lambda (f &rest args)
              "Report process state transitions, kill unneeded buffer"
              (let (r b)
                (prog1 (setq r (apply f args))
                  (setq b (asc-buffer r))
                  (when-let ((p (get-buffer-process b)))
                    (set-process-sentinel
                     (get-process p)
                     `(lambda (p e)
                        (message "%s `%s` at %s"
                                 (propertize
                                  (format "[%s]" (string-trim-right e))
                                  'face 'shadow)
                                 (propertize ,(car args) 'face 'success)
                                 (propertize
                                  ,(abbreviate-file-name default-directory)
                                  'face 'completions-annotations))
                        (unless (member (car (string-split e))
                                        '("stopped" "run"))
                          (read-only-mode -1)
                          (unless (member ,b (mapcar #'window-buffer
                                                     (window-list)))
                            (kill-buffer ,b))))))))))


;; Restart


(advice-add 'async-shell-command
            :around
            (lambda (f &rest args)
              "Store the command"
              (let ((r (apply f args)))
                (prog1 r
                  (with-current-buffer (asc-buffer r)
                    (setq-local command (car args)))))))


(defun asc-restart ()
  (interactive)
  (when-let ((p (get-buffer-process (current-buffer))))
    (kill-process p)
    (sit-for 0.5))
  (async-shell-command command (current-buffer)))


;; Display


(advice-add 'async-shell-command
            :around
            (lambda (f &rest args)
              "Don't display the buffer unless prefix arg is provided"
              (if current-prefix-arg
                  (apply f args)
                (let (r b)
                  (save-window-excursion
                    (prog1 (setq r (apply f args))
                      (setq b (asc-buffer r))
                      (switch-to-buffer b)
                      (message
                       "Running `%s` at %s"
                       (propertize (car args) 'face 'success)
                       (propertize (abbreviate-file-name default-directory)
                                   'face 'completions-annotations))))))))


;; Auxiliary commands


(defun asc-buffer (x)
  (if (windowp x) (window-buffer x) (process-buffer x)))


;; Keybindings


(with-eval-after-load 'shell
  (define-keymap :keymap shell-command-mode-map
    "g" 'asc-restart
    "q" 'quit-window))


;; ======
;; Comint
;; ======


(setq comint-pager "cat"
      comint-scroll-show-maximum-output nil
      comint-buffer-maximum-size (expt 2 13)
      comint-input-ring-size (expt 2 10)
      comint-input-ignoredups t
      comint-input-ring-separator "

")


(add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)


;; Persistent history


(defun comint-save-history ()
  (unless (zerop comint-command-count)
    (let* ((index (min comint-command-count
                       (ring-size comint-input-ring)))
           (commands (thread-first comint-input-ring
                                   ring-elements
                                   (seq-subseq 0 index)))
           (history (progn (setq comint-input-ring nil)
                           (comint-read-input-ring t)
                           (when comint-input-ring
                             (ring-elements comint-input-ring)))))
      (setq comint-input-ring (thread-first (nconc commands history)
                                            (cl-remove-duplicates
                                             :test #'equal :from-end t)
                                            ring-convert-sequence-to-ring))
      (ring-resize comint-input-ring comint-input-ring-size)
      (comint-write-input-ring))))


(put 'comint-command-count 'permanent-local t)


(defun comint-setup-persistent-history ()
  (when-let* (((not (eq major-mode 'shell-command-mode)))
              (process (get-buffer-process (current-buffer)))
              (histfile-id (thread-last
                             process process-command car
                             file-name-base downcase
                             (replace-regexp-in-string "[0-9]" "")
                             (replace-regexp-in-string
                              "^.*sh$\\|cmdproxy" "sh"))))
    (setq comint-input-ring-file-name
          (expand-file-name (format ".%s-history" histfile-id)
                            user-emacs-directory))
    (if (ring-empty-p comint-input-ring)
        (comint-read-input-ring t)
      (comint-save-history))
    (add-hook 'kill-buffer-hook 'comint-save-history nil t))
  (setq-local comint-command-count 0))


(add-hook 'comint-mode-hook 'comint-setup-persistent-history)


(advice-add 'comint-add-to-input-history
            :after-while
            (lambda (&rest _) (cl-incf comint-command-count)))


(defun comint-save-history-all ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when comint-input-ring-file-name
        (comint-save-history)))))


(add-hook 'kill-emacs-hook 'comint-save-history-all)


;; History browsing


(defun comint-browse-command-history ()
  (interactive)
  (let* ((history (ring-elements comint-input-ring))
         (b (current-buffer))
         (insert `(lambda ()
                    (interactive)
                    (icomplete-force-complete)
                    (let ((command (icomplete--field-string)))
                      (with-current-buffer ,b
                        (comint-delete-input)
                        (insert command)
                        (set-window-point (get-buffer-window) (point-max))))
                    (abort-recursive-edit)))
         (complete (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action
                        action history string pred))))
         (command (minibuffer-with-setup-hook
                      (:append (lambda ()
                                 (use-local-map
                                  (define-keymap :parent (current-local-map)
                                    "M-j" insert))))
                    (completing-read "Command history: " complete)))
         (i (cl-position command history :test #'equal)))
    (setq-local comint-input-ring-index i)
    (comint-delete-input)
    (insert command)
    (call-interactively (key-binding (kbd "RET")))))


;; Completion


(defun comint-setup-completion ()
  (setq-local company-backends '((company-capf company-dabbrev :separate))
              comint-dynamic-complete-functions
              (seq-difference comint-dynamic-complete-functions
                              '(comint-c-a-p-replace-by-expanded-history
                                shell-c-a-p-replace-by-expanded-directory
                                shell-command-completion
                                pcomplete-completions-at-point))))


(add-hook 'comint-mode-hook 'comint-setup-completion)


;; Keybindings


(with-eval-after-load 'comint
  (define-keymap :keymap comint-mode-map
    "C-c C-k" 'comint-kill-subjob
    "M-r" 'comint-browse-command-history))


;; =====
;; Shell
;; =====


(setq shell-prompt-pattern "^[^#$%>
]*#?[#$%>] *")


(defun shell-buffer-name ()
  (format "*shell%s*"
          (if-let ((host (file-remote-p default-directory 'host)))
              (concat "-" host) "")))


(defun shell-setup-buffer ()
  (or (and current-prefix-arg
           (let ((default-directory (read-directory-name "Shell at: ")))
             (generate-new-buffer (shell-buffer-name))))
      (when-let* ((project (project-current))
                  (d (project-root project)))
        (cl-find-if
         (lambda (x) (with-current-buffer x
                       (and (eq major-mode 'shell-mode)
                            (file-equal-p default-directory d))))
         (project-buffers project)))
      (get-buffer-create (shell-buffer-name))))


(advice-add 'shell
            :around
            (lambda (f &rest args)
              "Use `shell-setup-buffer'"
              (interactive (list (shell-setup-buffer)))
              (apply f args)))


;; Ssh sessions


(defun read-ssh-hosts ()
  (let* ((default-directory "~"))
    (cl-delete-duplicates
     (split-string (shell-command-to-string "c=~/.ssh/config; [ -f $c ] && sed -n -e '/Host \\*/ d' -e 's:Host ::p' $c"))
     :test #'equal)))


(defun ssh (host)
  "Establishes interactive ssh session"
  (interactive (list (completing-read "Run ssh session: "
                                      (read-ssh-hosts))))
  (let ((default-directory (format "/sshx:%s:" host))
        (explicit-shell-file-name "/bin/bash"))
    (shell (shell-buffer-name))))


;; Elevation of privilege


(defun shell-elevate ()
  "Elevates current shell session to root
   (equivalent to `sudo bash`).
   When called again, reverts to regular shell"
  (interactive)
  (let ((command (if (setq-local rootp (and (boundp 'rootp) rootp))
                     "exit"
                   "sudo $SHELL")))
    (comint-send-string
     (get-buffer-process (current-buffer))
     command)
    (comint-send-input)
    (setq-local rootp (not rootp))))


(with-eval-after-load 'shell
  (keymap-set shell-mode-map "C-x u" 'shell-elevate))


;; Completion


(defun sh-setup-completion ()
  (setq-local company-backends '((company-capf company-dabbrev-code :separate))
              completion-at-point-functions '(comint-completion-at-point t)))


(add-hook 'sh-mode-hook 'sh-setup-completion)


;; ===========
;; Web browser
;; ===========


;; EWW


(with-eval-after-load 'eww
  (when (eq system-type 'windows-nt)
    (setq eww-download-directory
          (expand-file-name "Downloads" (getenv "USERPROFILE")))))


(with-eval-after-load 'shr
  (setq shr-use-fonts nil
        shr-inhibit-images t))


;; External


(setq search-engines
      '(("wb". "https://www.wildberries.ru/catalog/0/search.aspx?search=%s")))


(defun browse-url-or-search (query)
  (interactive (list (read-string "Browse (WWW search): "
                                  (when (use-region-p)
                                    (prog1 (buffer-substring (region-beginning)
                                                             (region-end))
                                      (deactivate-mark)))
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


(setq ffap-url-fetcher
      (lambda (x)
        (browse-url-or-search x)
        (add-to-history 'browser-query-history x)))


;; ===================
;; EN ⇔ RU translator
;; ===================


(defun translate-en-ru (query)
  "Translate QUERY from english to russian (or vice versa)"
  (interactive (list (or (and (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end)))
                         (read-string "Translate: " (word-at-point)))))
  (let* ((default-directory "~")
         (query-encoded (url-encode-url (replace-regexp-in-string "'" "" query)))
         (query-message (propertize query 'face 'font-lock-constant-face))
         (en-ru `((description . "from English to Russian")
                  (command . ,(concat "bash -c \"curl -sL -A 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:128.0) Gecko/20100101 Firefox/128.0' '%s"
                                      "' | sed -rn '/span class=.trans/ {s:.*<span.*>(.*[^ ]) *<.span>.*:\\1:g ; p}'"
                                      " | uniq | head -5\""))
                  (link . ,(format "https://dictionary.cambridge.org/search/direct/?datasetsearch=english-russian&q=%s"
                                   query-encoded))))
         (ru-en `((description . "from Russian to English")
                  (command . ,(concat "bash -c \"curl -sL '%s"
                                      "' | grep -oP '(?<=class=.tl.>)[^<]+' | head -5 | tail -n +2\""))
                  (link . ,(format "https://en.openrussian.org/ru/%s"
                                   query-encoded))))
         (preset (if (string-match "[a-zA-Z]" query)
                     en-ru ru-en))
         (link (cdr (assoc 'link preset)))
         (command (format (cdr (assoc 'command preset)) link))
         translation)
    (message "%s =>\n%s"
             query-message
             (propertize "translating..." 'face 'shadow))
    (setq translation (string-trim (shell-command-to-string command)))
    (if (zerop (length translation))
        (let ((gptel-backend mistral)
              (gptel-model 'mistral-medium-latest)
              (prompt (format "Translate %s: %s"
                              (cdr (assoc 'description preset))
                              query))
              (callback `(lambda (response _)
                           (message "%s =>\n%s" ,query-message response))))
          (require 'gptel)
          (gptel-request prompt :callback callback))
      (message "%s =>\n%s" query-message translation))))


;; ====================
;; Cambridge dictionary
;; ====================


(defun camd (word)
  "Retrieves word definitions from the Cambridge Dictionary"
  (interactive (list (read-string "Find in Cambridge dictionary: "
                                  (word-at-point))))
  (let* ((query (propertize word 'face 'font-lock-constant-face))
         (answer (progn
                   (message "%s =>\n%s" query
                            (propertize "searching dictionary..."
                                        'face 'shadow))
                   (shell-command-to-string
                    (format "curl -sLA 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:128.0) Gecko/20100101 Firefox/128.0' https://dictionary.cambridge.org/dictionary/english/%s | sed -e '0,/Meaning/ d' -e 's:<[^>]*>: :g; s:</[^>]*>: :g; s:  *: :g' -e '/^ *$/ d' -e '/<a/ d; /if(typeof/,$ d' -e '0,/login-sign-in/ d; /Add to word list/ d' -e '/SMART Vocabulary/,$ d' -e '/{ .src.: ..images/ d; /GettyImages/ d; /Thesaurus: / d; / / d; /on=/ d; /open.>/ d; /See more/ d; s:&#[0-9][0-9]*;::g; s: *[A-Z][0-9]* ::g; s: *\\[\\] *::g; s: ,:,:g; s: \\.:\\.:g; s:^ *::g' | sed  -e '/More examples Fewer examples/,$ d'"
                            (downcase word))))))
    (if (string-empty-p answer)
        (message "Can't find definition of %s" query)
      (message "%s =>\n%s" query answer))))


;; ===============
;; Man page reader
;; ===============


(with-eval-after-load 'man
  (setq Man-support-remote-systems t
        Man-notify-method 'pushy))


;; ===========
;; ePub reader
;; ===========


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(with-eval-after-load 'nov
  (setq nov-text-width 70))


;; ===============
;; LLM integration
;; ===============


(setq mistral (gptel-make-openai "MistralLeChat"
                :host "api.mistral.ai"
                :endpoint "/v1/chat/completions"
                :protocol "https"
                :key 'mistral-api-key
                :models '(codestral-latest
                          mistral-medium-latest))
      gptel-backend mistral
      gptel-model 'mistral-medium-latest
      gptel--system-message "You are a large language model and a conversation partner. Respond concisely."
      gptel--set-buffer-locally t
      gptel-default-mode 'org-mode
      gptel-prompt-prefix-alist '((org-mode . "* "))
      gptel-track-media t
      gptel-include-reasoning nil
      gptel-expert-commands t)


;; The Chat


(setq gptel-chat-buffer-name "*LLM-chat*")


(defun gptel-chat ()
  "\"Just drop me into LLM chat, now!\"
Also grabs a selected region, if any."
  (interactive)
  (let* ((buffer-name gptel-chat-buffer-name)
         (exists-p (get-buffer buffer-name))
         (region (when (use-region-p)
                   (prog1 (buffer-substring
                           (region-beginning)
                           (region-end))
                     (deactivate-mark)))))
    (gptel buffer-name nil region t)
    (when (and region exists-p)
      (with-current-buffer buffer-name
        (end-of-buffer)
        (insert " " region)))))


(defun gptel-chat-setup ()
  (when (string-prefix-p gptel-chat-buffer-name
                         (buffer-name))
    (setq-local gptel-backend mistral
                gptel-model 'mistral-medium-latest)))


(add-hook 'gptel-mode-hook 'gptel-chat-setup)


;; Enable "coder" model in programming modes


(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local gptel-model 'codestral-latest)))


;; Rewrite facility


(with-eval-after-load 'gptel-rewrite
  (require 'diff-mode)
  (custom-set-faces
   '(gptel-rewrite-highlight-face ((t (:inherit diff-added)))))
  (define-keymap :keymap gptel-rewrite-actions-map
    "=" 'gptel--rewrite-diff
    "r" 'gptel--rewrite-iterate
    "SPC" 'gptel--rewrite-accept
    "m" 'gptel--rewrite-merge
    "k" 'gptel--rewrite-reject))


;; ==========================
;; Statistics / combinatorics
;; ==========================


(defun average (xs)
  "Get the arithmetic mean of a list of numbers"
  (/ (apply #'+ xs)
     (* 1.0 (length xs))))


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


;; ===========
;; Org Mode 🦄
;; ===========


(setq org-startup-truncated nil
      org-adapt-indentation t)


;; Capturing


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


;; Store org files in Git repo


(defun org-push ()
  (interactive)
  (let ((default-directory org-directory))
    (message "Pushing org repository...")
    (shell-command "git add * && git commit -m 'Updated' && git push && touch *.org")))


(defun org-pull ()
  (interactive)
  (let ((default-directory org-directory))
    (message "Pulling org repository...")
    (shell-command "git pull")))


;; Export


(setq org-export-with-section-numbers 0
      org-export-preserve-breaks t
      org-export-with-toc nil
      org-export-with-sub-superscripts nil)


;; Checklist


(defun org-insert-checklist-status ()
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (insert (if current-prefix-arg " [%]" " [/]"))
    (org-ctrl-c-ctrl-c)))


;; Keybindings


(with-eval-after-load 'org
  (define-keymap :keymap org-mode-map
    "C-c C-/" 'org-insert-checklist-status
    "M-c" (define-keymap :parent text-edit-map
            "," (lambda () (interactive) (enclose-text "=" "="))
            "." (lambda () (interactive) (enclose-text "~" "~"))
            "/" (lambda () (interactive) (enclose-text "/" "/"))
            "M-," (lambda () (interactive) (enclose-text "_" "_"))
            "M-." (lambda () (interactive) (enclose-text "+" "+")))))


;; Agenda


(with-eval-after-load 'org
  (setq org-agenda-files (list org-directory)))


;; Babel


(setq org-confirm-babel-evaluate nil
      org-babel-default-header-args:shell '((:results . "output")))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (lisp . t)
   (js . t)))


;; Cartesian product from org table


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


;; Permutations of data from org table


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


;; Allpairs


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
  (let ((allpairs (or (executable-find "allpairs")
                      (error "Allpairs executable not found. You should get it from https://www.satisfice.com/download/allpairs and make it available in exec-path"))))
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


;; List-like table view


(defun org-table-to-list ()
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


;; Fix in-table typing


(defconst org-table--separator-space-pre " ")


(defconst org-table--separator-space-post " ")


;; ===
;; VCS
;; ===


;; Good-looking mode line indicator


(setq vc-display-status 'no-backend)


(advice-add 'vc-mode-line-state
            :filter-return
            (lambda (s)
              "Prettify mode line indicator"
              (prog1 s
                (cl-case (cadr s)
                  (vc-up-to-date-state (setf (caddr s) "✓ "))
                  (vc-edited-state (setf (caddr s) "● "))))))


;; Informative branch indicator in Git's vc-dir


(defun update-vc-git-dir-extra-headers (headers)
  (let* ((status (string-trim-right
                  (shell-command-to-string
                   "git branch -v | sed -n 's:^\* ::p' | sed 's:  *: :'")))
         (p1 (string-match-p "\\[\\(ahead\\|behind\\)" status))
         (p2 (when p1 (string-match-p "\\]" status p1))))
    (set-text-properties
     0 (length status) '(face vc-dir-header-value) status)
    (when p1
      (set-text-properties
       0 (1- p1) '(face vc-dir-header-value) status)
      (set-text-properties
       p1 (1+ p2) '(face success) status)
      (set-text-properties
       (1+ p2) (length status) '(face vc-dir-header-value) status))
    (replace-regexp-in-string
     "\\(Branch.*: \\).*"
     (format "\\1%s" status) headers)))


(advice-add 'vc-git-dir-extra-headers
            :filter-return
            #'update-vc-git-dir-extra-headers)


;; Update vc-dir buffer after commit


(defun vc-refresh-headers ()
  (ewoc-set-hf vc-ewoc (vc-dir-headers vc-dir-backend default-directory) ""))


(defun vc-dir-log-edit-update ()
  (when-let ((buffer (cl-find-if
                      (lambda (x) (with-current-buffer x
                                    (eq major-mode 'vc-dir-mode)))
                      (when-let ((project (project-current)))
                        (project-buffers project)))))
    (with-current-buffer buffer
      (run-with-timer 0.01 nil
                      `(lambda ()
                         (with-current-buffer ,buffer
                           (vc-refresh-headers)))))))


(add-hook 'log-edit-done-hook 'vc-dir-log-edit-update)


;; Explicitly define some commands


(setq vc-command-overrides
      '((vc-pull . ((Git . "git pull")))
        (vc-push . ((Git . "git push")))))


(dolist (vc-command '(vc-pull vc-push))
  (advice-add vc-command
              :around
              `(lambda (f &rest args)
                 (if-let* ((backend (car (vc-deduce-fileset t)))
                           (o (assoc ',vc-command vc-command-overrides))
                           (command (cdr (assoc backend o))))
                     (progn (message "Running %s..."
                                     (propertize command 'face 'success))
                            (shell-command command)
                            (cond ((eq major-mode 'vc-dir-mode)
                                   (vc-refresh-headers))
                                  ((derived-mode-p 'log-view-mode)
                                   (revert-buffer))))
                   (apply f args)))))


;; Reset


(defun vc-reset (&optional args action)
  (interactive)
  (let* ((default "HEAD^")
         (dir (expand-file-name (vc-root-dir)))
         (revision (vc-read-revision (format "%s to (default %s): "
                                             (or action "Reset")
                                             default)
                                     (list dir)
                                     (vc-responsible-backend dir)
                                     default)))
    (shell-command (format "git reset %s %s"
                           (or args "")
                           revision)))
  (vc-dir-refresh))


(defun vc-reset-hard ()
  (interactive)
  (vc-reset "--hard" "Hard reset"))


;; Convenient revision copying


(defun log-view-copy-revision ()
  (interactive)
  (let ((r (cadr (log-view-current-entry))))
    (kill-new r)
    (message "Copied to clipboard: %s" r)))


;; Commit message history


(add-to-list 'savehist-additional-variables 'log-edit-comment-ring)


;; Keybindings


(with-eval-after-load 'vc-dir
  (define-keymap :keymap vc-dir-mode-map
    "TAB" 'vc-dir-next-line
    "<backtab>" 'vc-dir-previous-line
    "l" 'vc-print-root-log
    "s" 'vc-switch-branch
    "c" 'vc-create-branch
    "r" 'vc-reset
    "R" 'vc-reset-hard))


(with-eval-after-load 'log-view
  (define-keymap :keymap log-view-mode-map
    "j" 'vc-print-branch-log
    "w" 'log-view-copy-revision))


(with-eval-after-load 'log-edit
  (keymap-unset log-edit-mode-map "M-s"))


;; ========
;; Projects
;; ========


(setq project-switch-commands
      '((project-dired "Open project root")
        (project-find-file "Find file")
        (project-find-dir "Find directory")
        (project-find-regexp "Find regexp")
        (project-vc-dir "View VCS status")
        (project-shell "Shell")
        (project-async-shell-command "Run async shell command")
        (project-compile "Compile")))


(add-hook 'emacs-startup-hook 'project-forget-zombie-projects)


(defun project-try-file (dir)
  (cl-loop for pattern in '("pom.xml" ; Java
                            "*.iml"
                            "build.xml"
                            "build.gradle"
                            ".project"
                            "project.clj" ; Clojure
                            "deps.edn"
                            "Cargo.toml" ; Rust
                            "go.mod" ; Go
                            "Makefile" ; C/C++
                            "requirements.txt" ; Python
                            "venv"
                            "package.json"; Javascript
                            "*.dlrproj" ; Weird things
                            )
           for project-file = (locate-dominating-file
                               dir
                               (lambda (d)
                                 (car (file-expand-wildcards
                                       (expand-file-name pattern d)))))
           when project-file
           return (cons 'project-file
                        (file-name-directory project-file))))


(cl-defmethod project-root ((project (head project-file)))
  (cdr project))


(setq project-find-functions '(project-try-vc project-try-file))


;; ===========
;; Compilation
;; ===========


(require 'ansi-color)


(defun compilation-enable-ascii-codes ()
  (ansi-color-apply-on-region (point-min) (point-max)))


(add-hook 'compilation-filter-hook 'compilation-enable-ascii-codes)


(setq compilation-scroll-output t)


(advice-add 'compilation-read-command
            :around
            (lambda (f &rest args)
              "Don't suggest initial input when reading the command"
              (apply f (cons nil (cdr args)))))


;; ===
;; LSP
;; ===


(with-eval-after-load 'eglot
  ;; Keybindings
  (define-keymap :keymap eglot-mode-map
    "M-p" 'eglot-code-actions
    "M-." 'xref-find-definitions
    "C-c C-n" 'eglot-rename
    "C-c C-e" 'eglot-code-action-extract
    "C-h C-h" 'eldoc-print-current-symbol-info
    "C-c C-i" 'eglot-code-action-inline
    "C-c C-j" 'eglot-code-action-quickfix
    "C-c C-k" 'eglot-code-action-rewrite
    "C-c C-l" 'eglot-code-action-organize-imports)

  ;; Don't clutter company settings
  (add-to-list 'eglot-stay-out-of 'company)

  ;; Auto-shutdown the server
  (setq eglot-autoshutdown t)

  ;; Turn off flymake mode on exit
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (unless (eglot-managed-p)
                (flymake-mode -1)))))


;; =======
;; Flymake
;; =======


(with-eval-after-load 'flymake
  (define-keymap :keymap flymake-mode-map
    "C-," 'flymake-goto-prev-error
    "C-." 'flymake-goto-next-error
    "C-c C-o" 'flymake-show-buffer-diagnostics
    "C-c C-p" 'flymake-show-project-diagnostics))


(dolist (x '(flymake-show-buffer-diagnostics
             flymake-show-project-diagnostics))
  (advice-add x :around
              (lambda (f &rest args)
                "Fix undesired layout of diagnostic buffers"
                (let (b)
                  (save-window-excursion
                    (setq b (window-buffer (apply f args))))
                  (display-buffer b)))))


;; =====
;; ElDoc
;; =====


(setq eldoc-echo-area-use-multiline-p nil)


;; Fix garbled appearance


(advice-add 'eldoc--format-doc-buffer
            :around
            (lambda (f &rest args)
              "Fix garbled appearance"
              (let ((b (apply f args)))
                (prog1 b
                  (with-current-buffer b
                    (let ((inhibit-read-only t))
                      (save-excursion
                        (goto-char 1)
                        (while (re-search-forward "" nil t)
                          (replace-match "")))))))))


;; Fix link navigation


(defun eldoc-url-at-point ()
  (if (eq (get-text-property (point) 'face)
          'markdown-plain-url-face)
      (ffap-url-at-point)
    (get-text-property (point) 'help-echo)))


(defun eldoc-open-url-at-point ()
  (interactive)
  (if-let (url (eldoc-url-at-point))
      (progn (message "Opening doc: %s..." url)
             (browse-url-or-search url))
    (message "No references at point")))


(defun eldoc-make-nav-link-command (prop-change-fn)
  `(lambda ()
     (interactive)
     (let (p found)
       (save-excursion
         (while (and (setq p (funcall ',prop-change-fn (point)))
                     (goto-char p)
                     (let ((prop (get-text-property p 'face)))
                       (not (setq found
                                  (if (listp prop)
                                      (memq 'markdown-link-face prop)
                                    (memq prop
                                          '(markdown-link-face
                                            markdown-plain-url-face)))))))))
       (when found
         (goto-char p)
         (message (eldoc-url-at-point))))))


(defun eldoc-fix-link-navigation (f &rest args)
  (let ((b (apply f args)))
    (prog1 b
      (with-current-buffer b
        (use-local-map
         (define-keymap :parent (current-local-map)
           "RET" 'eldoc-open-url-at-point
           "TAB" (eldoc-make-nav-link-command
                  'next-property-change)
           "<backtab>"(eldoc-make-nav-link-command
                       'previous-property-change)))))))


(advice-add 'eldoc--format-doc-buffer :around #'eldoc-fix-link-navigation)


;; ===========
;; ClangFormat
;; ===========


(setq clang-format (executable-find "clang-format"))


(defun clang-format-buffer ()
  (interactive)
  (let* ((extension (or (file-name-extension (or (buffer-file-name) ""))
                        (replace-regexp-in-string "-mode" "" (symbol-name major-mode))))
         (style "'{IndentWidth: 4}'"))
    (format-buffer
     (format "%s --assume-filename=.%s --style=%s" clang-format extension style))))


(when clang-format
  (dolist (x '(c-mode java-mode js-mode))
    (add-to-list 'format-buffer-functions (cons x 'clang-format-buffer))))


;; ========
;; Prettier
;; ========


(defun prettier-create-config ()
  "Write config into ~/.prettierrc file unless it already exists"
  (interactive)
  (when-let* ((config-file (expand-file-name ".prettierrc"
                                             (if (eq system-type 'windows-nt)
                                                 (getenv "USERPROFILE")
                                               "~")))
              ((not (file-exists-p config-file)))
              (java-plugin (expand-file-name
                            "prettier-plugin-java/dist/index.js"
                            (string-trim
                             (shell-command-to-string "npm root -g"))))
              (config `((printWidth . 100)
                        (overrides
                         . (((files . "*.java")
                             (options . ((tabWidth . 4)
                                         (plugins . (,java-plugin))))))))))
    (with-temp-buffer
      (insert (json-encode config))
      (json-pretty-print-buffer)
      (write-file config-file))))


(defun prettier ()
  "Format current buffer with Prettier"
  (interactive)
  (let* ((fname (buffer-file-name))
         (default-directory "~")
         (mode (unless fname
                 (replace-regexp-in-string
                  "-mode" "" (symbol-name major-mode))))
         (parser (unless fname
                   (cl-find mode
                            (setq-local
                             prettier-parsers
                             (or (bound-and-true-p prettier-parsers)
                                 (string-split
                                  (with-temp-buffer
                                    (insert (shell-command-to-string "prettier -h"))
                                    (goto-char 1)
                                    (buffer-substring (search-forward "--parser <")
                                                      (1- (search-forward ">"))))
                                  "|")))
                            :test #'equal))))
    (format-buffer
     (format
      "prettier --no-color %s"
      (cond (fname (format "--stdin-filepath file.%s"
                           (file-name-extension fname)))
            (parser (format "--parser %s" parser))
            (t (format "--stdin-filepath file.%s" mode)))))))


(when (executable-find "prettier")
  (prettier-create-config)
  (dolist (m '(js-mode java-mode mhtml-mode html-mode css-mode))
    (add-to-list 'format-buffer-functions (cons m 'prettier))))


;; ===
;; XML
;; ===


(with-eval-after-load 'html-mode-expansions
  (er/enable-mode-expansions 'sgml-mode #'er/add-html-mode-expansions))


(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "xslt" "xsl" "wsdl" "xml.template" "pom" "jmx") t) "\\'") 'sgml-mode))


(setq xmllint (executable-find "xmllint"))


(defun xml-format-buffer ()
  (interactive)
  (format-buffer (format "%s --format -" xmllint)))


(when xmllint
  (add-to-list 'format-buffer-functions '(sgml-mode . xml-format-buffer)))


;; ====
;; Java
;; ====


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


(with-eval-after-load 'cc-mode
  (keymap-set java-mode-map "C-c C-c" 'copy-java-class-full-name))


;; =======
;; Clojure
;; =======


(setq cider-repl-history-file (expand-file-name ".cider-history"
                                                user-emacs-directory)
      cider-show-error-buffer nil)


(with-eval-after-load 'cider-repl
  (keymap-unset cider-repl-mode-map "M-s"))


;; ===========
;; Common lisp
;; ===========


(add-to-list 'auto-mode-alist
             '("\\.cl\\'" . common-lisp-mode))


(defun use-eww-for-cl-hyperspec-lookup ()
  (setq-local browse-url-browser-function 'eww-browse-url))


(defun common-lisp-setup-company ()
  (require 'slime-company)
  (setq-local company-backends (cons 'company-slime (cdr company-backends))))


(dolist (m '(lisp-mode-hook slime-repl-mode-hook))
  (add-hook m 'use-eww-for-cl-hyperspec-lookup)
  (add-hook m 'common-lisp-setup-company))


;; ==========
;; Powershell
;; ==========


(advice-add 'powershell
            :after
            (lambda (&rest _)
              "Set encoding to cp866"
              (set-buffer-process-coding-system 'cp866-dos 'cp866-dos)))


;; ==========
;; SQL client
;; ==========


(setq sql-connection-alist '((sqlilte (sql-product 'sqlite))
                             (sqlilte-in-memory (sql-product 'sqlite)
                                                (sql-database ""))
                             (firebird (sql-product 'interbase)
                                       (sql-user "sysdba")
                                       (sql-password "masterkey"))))


;; Fix rough corners


(defun ignore-sql-database (f &rest args)
  (let ((sql-database ""))
    (apply f args)))


(advice-add 'sql-connect :around #'ignore-sql-database)


;; Initial setup


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


;; Reconnect


(defun sql-setup-reconnect ()
  (let ((process (get-buffer-process (current-buffer))))
    (setq-local process-specs
                (list (process-name process)
                      (process-command process)))))


(add-hook 'sql-login-hook 'sql-setup-reconnect)


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


;; Deal with remote dbs


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


(advice-add 'sql-comint :around #'sql-handle-remote-db)


;; Output preprocessing


(add-hook 'sql-login-hook 'orgtbl-mode)


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


(add-hook 'sql-login-hook 'sql-setup-output-preprocessing)


;; Keybindings


(with-eval-after-load 'sql
  (define-keymap :keymap sql-interactive-mode-map
    "C-c C-j" 'sql-reconnect
    "C-c C-p" 'org-table-to-list))


;; Interbase


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


(setq sql-interbase-login-params '(user password (database :file)))


(with-eval-after-load 'sql
  (sql-set-product-feature 'interbase :table-parser 'parse-isql-table)
  (sql-set-product-feature 'interbase :init-commands '("set list on;")))


;; Sqlite


(defun parse-sqlite-table (text &optional ignored)
  (unless (string-match "^Error: " text)
    (list (mapcar (lambda (r) (mapcar #'string-trim (split-string r "|")))
                  (let ((lines (split-string text "\n" t)))
                    (if (string-match "^select .*;$" (car lines))
                        (cdr lines)
                      lines)))
          "")))


(with-eval-after-load 'sql
  (add-to-list 'sql-sqlite-options "-interactive")
  (sql-set-product-feature 'sqlite :init-commands '(".headers on"))
  (sql-set-product-feature 'sqlite :table-parser 'parse-sqlite-table))


;; ===========
;; File server
;; ===========


(defun serve-directory ()
  (interactive)
  (let* ((default-directory (read-directory-name "Serve directory: "))
         (socket (read-string "Address: " "0.0.0.0:5555"))
         (command (apply 'format "python -m http.server -b %s %s" (split-string socket ":")))
         (buffer (format "*python-server:%s*" socket)))
    (async-shell-command command buffer)
    (message "Serving %s at %s"
             (propertize (abbreviate-file-name default-directory)
                         'face 'completions-annotations)
             (propertize socket 'face 'bold))))


;; ===============
;; Video capturing
;; ===============


(defun capture-video (arg)
  (interactive "P")
  (let ((ffmpeg (or (executable-find "ffmpeg")
                    (error "ffmpeg executable not found")))
        (buffer-name "*video-capture*")
        (capture-file-name (if (eq system-type 'windows-nt)
                               (expand-file-name "Videos/v.mp4" (getenv "USERPROFILE"))
                             "~/v.mp4")))
    (if (get-buffer-process buffer-name)
        (with-current-buffer buffer-name
          (message "Captured: %s" (propertize capture-file-name 'face 'font-lock-constant-face))
          (kill-new capture-file-name)
          (comint-interrupt-subjob)
          (sit-for 1)
          (kill-buffer))
      (let* ((capture-file-name (if arg
                                    (read-file-name "Capture video to file: ")
                                  capture-file-name))
             (default-directory (file-name-directory capture-file-name))
             (command (if (eq system-type 'windows-nt)
                          "%s -y -f gdigrab -i desktop -framerate 30 -pix_fmt yuv420p %s"
                        "%s -y -f x11grab -i :0.0 -framerate 30 -pix_fmt yuv420p %s")))
        (async-shell-command
         (format command
                 ffmpeg
                 (car (last (file-name-split capture-file-name))))
         buffer-name)
        (with-current-buffer buffer-name
          (setq-local capture-file-name capture-file-name))
        (message "Capturing video to file: %s"
                 (propertize capture-file-name 'face 'font-lock-constant-face))))))
