;; Map an nlspec org file to DSL code
;; - nlspec code blocks are copied verbatim
;; - tables are converted to nlspec doe

;; The DSL is implemented as a Haskell monad.

;; Some conventions:
;; - table names, column names need to be representable as haskell function names
;; - same for cell contents, but after '_' prefix

(defun nlspec-compile ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (basename (file-name-sans-extension filename))
         (modulename (file-name-nondirectory basename))
         (name (concat basename ".hs"))
         (buf (or (get-buffer name)
                  (save-excursion (find-file-noselect name)))))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert (format "-- src: %s\n" filename))
      (insert "module ")
      (insert modulename)
      (insert " where\n")
      (insert "import ")
      (insert modulename)
      (insert "Lib\n")
      (insert "nlspec = do\n"))
    ;; Table definitions
    (org-table-map-tables
     (lambda ()
       (let* ((name
               (org-element-property 
                :name 
                (org-element-at-point)))
              (table
               (remove-if
                (lambda (row) (eq row 'hline))
                (org-table-to-lisp)))
              (header (car table))
              (rows (cdr table)))
         ;; FIXME: Convert to hs syntax.
         (save-excursion
           (set-buffer buf)
           (end-of-buffer)
           (nlspec-insert-table name header rows)
           ))))
    ;; Insert code last, as it might reference the tables
    (org-babel-map-src-blocks nil
      (save-excursion
        (set-buffer buf)
        (end-of-buffer)
        (dolist (line (split-string body "\n"))
          (insert "  ")
          (insert line)
          (insert "\n")
        )))
    (save-excursion
      (set-buffer buf)
      (end-of-buffer)
      (haskell-mode)
      (basic-save-buffer)
      )
    ;; Prev code does messages anyway so just be loud.
    (message (format "nlspec-compile: %s" name))
    ))

;; (defun my-insert-nlspec-table (name header rows)
;;   (insert (pp-to-string (list 'table name header rows))))

(defun nlspec-insert-table (name header rows)
  (dolist (row rows)
    ;; Note that module needs to expose bind1, bind2, ...
    (insert (format "  bind%d " (length header)))
    (insert name)
    (let ((hrs (mapcar* 'list header row)))
      (dolist (hr hrs)
        (let ((type (car hr))
              (tag  (cadr hr)))
          (insert " (")
          (insert type)
          (insert " ")
          (insert (pp-to-string tag))
          (insert ")")
          )))
    (insert "\n")))

