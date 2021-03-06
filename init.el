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


;; ============
;; key bindings
;; ============


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
  (progn (define-prefix-command 'run-cider-map)
         (define-key 'repls-map (kbd "j") 'run-cider-map)
         (define-key 'run-cider-map (kbd "k") 'cider-connect)
         (define-key 'run-cider-map (kbd "j") 'cider-jack-in)))


;; extending global search map


(progn
  (define-key search-map (kbd "r") 'replace-string)
  (define-key search-map (kbd "R") 'replace-regexp)
  (define-key search-map (kbd "t") 'translate-en-ru-online))


;; transforming text


(progn
  (define-prefix-command 'text-transform-map)
  (global-set-key (kbd "M-c") 'text-transform-map)
  (define-key 'text-transform-map (kbd "c") 'upcase-char)
  (define-key 'text-transform-map (kbd "M-d") 'downcase-dwim)
  (define-key 'text-transform-map (kbd "M-c") 'upcase-dwim)
  (define-key 'text-transform-map (kbd "/") 'invert-slashes)
  (define-key 'text-transform-map (kbd "j") 'join-region)
  (define-key 'text-transform-map (kbd "b") 'break-line))


;; global keymap


(let ((bindings
       '(("M-x" smex)
         ("C-x C-b" ibuffer)
         ("C-c p" copy-file-name-to-clipboard)
         ("C-c d" duplicate-line)
         ("M-k" kill-whole-line)
         ("C-x u" insert-char)
         ("M-q" hippie-expand)
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


;; =====
;; files
;; =====


;; no backups


(setq auto-save-default nil)


(setq make-backup-files nil)


(setq auto-save-list-file-name nil)


;; reverting file-related buffers


(global-auto-revert-mode t)


(when (string-equal system-type "windows-nt")
  (setq auto-revert-use-notify nil))


(defun force-revert-buffer ()
  (interactive)
  (message "Force revert buffer '%s'" (buffer-name))
  (revert-buffer nil t t))


;; open file(s) in external app


(defun open-in-external-app (&optional file-name)
  (interactive)
  (let ((open-file
         (cond ((string-equal system-type "windows-nt")
                (lambda (f) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" f t t))))
               ((string-equal system-type "darwin")
                (lambda (f) (shell-command (concat "open " (shell-quote-argument f)))))
               ((string-equal system-type "gnu/linux")
                (lambda (f) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" f)))))))
    (funcall open-file file-name)))


;; dired


(setq ls-lisp-format-time-list
      '("%d.%m.%Y %H:%M"
        "%d.%m.%Y %H:%M")
      ls-lisp-use-localized-time-format t)


(setq dired-listing-switches "-alh"
      dired-recursive-copies 'always
      dired-dwim-target t)


(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))


(defun dired-open-in-external-app ()
  (interactive)
  (mapc #'open-in-external-app (dired-get-marked-files)))


(with-eval-after-load 'dired
  (define-key dired-mode-map
    (kbd "C-c C-o")
    'dired-open-in-external-app))


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


(defun join-region (separator)
  (interactive "sSeparator: ")
  (when (region-active-p)
    (let ((text (buffer-substring
                 (region-beginning)
                 (region-end))))
      (setq text (split-string text "\n" t))
      (setq text (string-join text separator))
      (delete-active-region)
      (insert text))))


(defun break-line (separator)
  (interactive "sSeparator: ")
  (let ((text (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (setq text (split-string text separator t))
    (setq text (string-join text "\n"))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert text)))


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
           ("SQL" (mode . sql-mode))
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
           ("Scheme" (mode . scheme-mode))
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
       (require 'org-tempo))


(setq org-startup-truncated nil)


(setq org-capture-templates
      `(("w" "Work" entry
         (file+headline "work.org" "inbox")
         "* TODO %?\n")
        ("f" "Family" entry
         (file+headline "family.org" "inbox")
         "* TODO %?\n")
        ("e" "Education" entry
         (file+headline "education.org" "inbox")
         "* TODO %?\n")
        ("t" "Tools" entry
         (file+headline "tools.org" "inbox")
         "* TODO %?\n")))


(setq org-refile-allow-creating-parent-nodes 'confirm)


(setq org-export-with-section-numbers 0)


;; babel


(setq org-confirm-babel-evaluate nil)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))


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
      (let ((pname (cond ((and (equal major-mode 'shell-mode)
                               (string-match "powershell" (car (process-command process))))
                          "powershell")
                         (t (replace-regexp-in-string
                             "<[0-9]*>" ""
                             (process-name process))))))
        (setq-local comint-input-ring-file-name
                    (expand-file-name (format ".%s-history" pname)
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
  (when (derived-mode-p 'comint-mode)
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
    (comint-read-input-ring)))


(defun comint-save-history-all ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (comint-save-history))))


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


;; ===
;; xml
;; ===


;; use sgml mode for xml files


(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "xslt" "xsl" "html" "htm" "wsdl" "xml.template" "xhtml" "jsp" "pom" "jmx") t) "\\'") 'sgml-mode))


(setq sgml-basic-offset 4)


(defun html-dummy (title)
  (interactive "sTitle: ")
  (insert (format "<!DOCTYPE html>
<html>
<head>
    <title>%s</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
</head>
<body>


</body>
</html>" title)))


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


(progn
  (define-key isearch-mode-map (kbd "M-q") 'isearch-query-replace)
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
      ;; Example config
      `(("bash" .
         ((file-name . "c:/tools/msys64/usr/bin/bash.exe")))
        ("powershell"
         (startup-fn . powershell)
         (codings . (cp866-dos cp866-dos)))))


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
         (en-ru (concat "bash -c \"curl -sL "
                        (format "'https://dictionary.cambridge.org/search/direct/?datasetsearch=english-russian&q=%s'"
                                (url-encode-url query))
                        " | sed -rn '/span class=.trans/ {s:.*<span.*>(.*[^ ]) *<.span>.*:\\1:g ; p}'"
                        " | sort | uniq\""))
         (ru-en (concat "bash -c \"curl -sL '"
                        (format "https://en.openrussian.org/ru/%s"
                                (url-encode-url query))
                        "' | grep -oP '(?<=class=.tl.>)[^<]+' | head -n 3\""))
         (command (if (string-match "[a-zA-Z]" query)
                      en-ru ru-en))
         (translation (shell-command-to-string command)))
    (message "%s" command)
    (if (zerop (length translation))
        (message "Can't find translation for '%s'" query)
      (message "%s =>\n%s" query translation))))


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
