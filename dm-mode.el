;;; dm-mode.el --- BYOND DreamMaker major mode.

;; Author: Sam Schweigel <s.schweigel@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (lsp-ui "6.0") (xml-parse "1.5"))
;; Keywords: byond

(require 'lsp-mode)
(require 'lsp-ui)
(require 'ivy)

(defvar dm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    st))

(defvar dm-mode-font-lock-keywords
  `(,(rx symbol-start
         (or
          "sleep" "spawn" "break" "continue" "do" "else"
          "for" "goto" "if" "return" "switch" "while"
          "del" "new")
         symbol-end)
    (,(rx line-start
          "#" (0+ nonl)
          line-end)
     . font-lock-preprocessor-face)
    (,(rx symbol-start
          (or
           "proc" "verb" "datum" "atom" "movable" "obj"
           "mob" "turf" "area" "savefile" "list" "client"
           "sound" "image" "database" "matrix" "regex" "exception"
           "as" "const" "global" "set" "static" "tmp")
          symbol-end)
     . font-lock-type-face)
    (,(rx symbol-start
          (or
           "usr" "world" "src" "args" "null"))
     . font-lock-constant-face)
    (,(rx symbol-start (opt "/") (0+ (1+ (or word ?_)) "/") (group (1+ (or word ?_))) "(")
     (1 font-lock-function-name-face))
    (,(rx symbol-start
          "var" (or "/" (1+ space))
          (group (1+ (or word ?_))))
     (1 font-lock-variable-name-face))
    (,(rx symbol-start
          (group (1+ (or word ?_))) (0+ space)
          "=")
     (1 font-lock-variable-name-face))))

(defun dm-indent-line-function ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (tab-to-tab-stop))
  (when (= (char-before) ?\n)
    (back-to-indentation)))

(defun dm-dedent-line-function ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (when (= (char-before) ?\t)
      (delete-char -1))))

(defvar dm-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<backtab>") #'dm-dedent-line-function)
    (define-key m (kbd "<tab>") #'dm-indent-line-function)
    (define-key m (kbd "C-c C-c") #'dm-compile)
    (define-key m (kbd "C-c C-t") #'dm-tree)
    (define-key m (kbd "C-c C-r") #'dm-force-reparse-tags)
    m))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "dm-langserver")
                  :major-modes '(dm-mode)
                  :server-id 'dm-langserver))

(defvar dm-compile-command
  "DreamMaker")

(defun dm-get-dme ()
  (let* ((default-directory (or (lsp-workspace-root) default-directory))
         (dmes (directory-files default-directory nil ".*\\.dme"))
         (num-found (length dmes)))
    (cond
     ((= 1 (length dmes)) (car dmes))
     ((> 1 (length dmes)) (error "Too many DMEs."))
     (t (error "No DME found.")))))

(defun dm-compile ()
  (interactive)
  (let ((default-directory (or (lsp-workspace-root) default-directory)))
    (compile (concat dm-compile-command " " (dm-get-dme)))))

(defun dm-generate-tags ()
  (interactive)
  (let ((default-directory (or (lsp-workspace-root) default-directory))
        (dme (dm-get-dme)))
    (start-process "dm-tags" "*dm-tags*" dm-compile-command
                   "-o" dme)))

(defvar dm-tags nil)

(defun dm-force-reparse-tags ()
  (interactive)
  (setq dm-tags nil)
  (dm-get-tags))

(defun dm-get-tags ()
  (or
   dm-tags
   (let ((default-directory (or (lsp-workspace-root) default-directory)))
     (if (file-exists-p "dm-tags.xml")
         (let ((progress (make-progress-reporter "Parsing tags" 0 100)))
           (with-temp-buffer
             (insert-file-contents "dm-tags.xml")
             (setq dm-tags (read-xml (lambda (p) (progress-reporter-update progress p))))
             (progress-reporter-done progress))
           dm-tags)
       (error "Couldn't find DM tags (dm-tags.xml).")))))

(defun dm-goto-file-line (f)
  (string-match
   (rx (group (1+ (not (any ?:))))
      (char ?:)
      (group (1+ digit)))
   f)
  (let ((line (match-string 2 f))
        (buf (find-file-other-window (match-string 1 f))))
    (when line
      (goto-line (string-to-number line) buf))))

(defun dm-goto-link (x)
  `(link :tag ,(propertize "g" 'face 'link)
         :value ,(cdadar x)
         :notify ,(lambda (w &rest ignore)
                    (dm-goto-file-line (widget-value w)))))


(defun dm-face-category (c)
  (propertize
   c
   'face
   (cdr (assoc c '(("var" . font-lock-variable-name-face)
                   ("object" . font-lock-type-face)
                   ("proc" . font-lock-function-name-face)
                   ("area" . font-lock-constant-face)
                   ("obj" . font-lock-builtin-face)
                   ("turf" . font-lock-string-face)
                   ("mob" . font-lock-preprocessor-face)
                   ("verb" . font-lock-doc-face))))))

(defun dm-tree-widget (tree)
  (let ((categories (seq-filter
                     (lambda (x) (not (equal x "val")))
                     (delete-dups (mapcar #'caar tree)))))
    (mapcar (lambda (c)
              `(tree-widget
                :tag ,(dm-face-category c)
                ,@(mapcar
                   (lambda (x)
                     (if (and (cddr x)
                              (seq-filter (lambda (x) (not (equal (caar x) "val"))) (cddr x)))
                         `(tree-widget
                           :node (group ,(dm-goto-link x)
                                        (item ,(concat " " (string-trim (cadr x)))))
                           ,@(dm-tree-widget (cddr x)))
                       `(group
                         ,(dm-goto-link x)
                         (item ,(concat " " (string-trim (cadr x)))))))
                  (seq-filter (lambda (x) (equal (caar x) c))
                              tree))))
            categories)))

(defun dm-tree ()
  "Browse the DM object tree."
  (interactive)
  (unless dm-tags
    (dm-get-tags))
  (let ((default-directory (or (lsp-workspace-root) default-directory)))
    (switch-to-buffer "*dm-tree*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (mapcar (lambda (x)
              (apply #'widget-create x))
            (dm-tree-widget (cdr dm-tags)))
    (use-local-map widget-keymap)
    (widget-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dm\\'" . dm-mode))

;;;###autoload
(define-derived-mode dm-mode prog-mode "DM"
  "Major mode for BYOND DreamMaker.

\\{dm-mode-map}"

  :syntax-table dm-mode-syntax-table

  (set (make-local-variable 'indent-tabs-mode) t)
  (set (make-local-variable 'tab-width) 2)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (set (make-local-variable 'electric-indent-inhibit) t)
  (set (make-local-variable 'electric-indent-chars)
       (cons ?: electric-indent-chars))

  (set (make-local-variable 'font-lock-defaults)
       '(dm-mode-font-lock-keywords))

  (set (make-local-variable 'show-trailing-whitespace) nil)

  (use-local-map dm-mode-map)
  (company-mode 1)
  (lsp-ui-mode 1)
  (flycheck-mode 1)
  (lsp))

(provide 'dm-mode)

;;; dm-mode.el ends here
