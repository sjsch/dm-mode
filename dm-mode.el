;;; dm-mode.el --- BYOND DreamMaker major mode.

;; Author: Sam Schweigel <s.schweigel@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (lsp-ui "6.0") (ivy "0.10"))
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
     (1 font-lock-variable-name-face))
    (,(rx line-start
          "#" (0+ nonl)
          line-end)
     . font-lock-preprocessor-face)))

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
    m))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "dm-langserver")
                  :major-modes '(dm-mode)
                  :server-id 'dm-langserver))

(defvar dm-compile-command
  "DreamMaker")

(defun dm-compile ()
  (interactive)
  (let* ((default-directory (lsp-workspace-root))
         (dmes (directory-files default-directory nil ".*\\.dme"))
         (num-found (length dmes)))
    (cond
     ((= 1 (length dmes)) (compile (concat dm-compile-command " " (car dmes))))
     ((> 1 (length dmes)) (error "Too many DMEs."))
     (t (error "No DME found.")))))

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
