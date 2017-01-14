;;; Almost entirely ripped off from Projectile by bbatsov

(defvar test-suffixes '("_test" "_spec" "-test" "-spec" ".test" ".spec"))
(defvar test-prefixes '("test_" "spec_" "test-" "spec-"))
(defvar current-project-files-func 'projectile-current-project-files)
(defvar test-switcher-project-root-func 'projectile-project-root)

(defun test-switcher-dirname-matching-count (a b)
  "Count matching dirnames ascending file paths."
  (setq a (reverse (split-string (or (file-name-directory a) "") "/" t))
        b (reverse (split-string (or (file-name-directory b) "") "/" t)))
  (let ((common 0))
    (while (and a b (string-equal (pop a) (pop b)))
      (setq common (1+ common)))
    common))

(defun test-switcher-group-file-candidates (file candidates)
  "Group file candidates by dirname matching count."
  (cl-sort (copy-sequence
            (let (value result)
              (while (setq value (pop candidates))
                (let* ((key (test-switcher-dirname-matching-count file value))
                       (kv (assoc key result)))
                  (if kv
                      (setcdr kv (cons value (cdr kv)))
                    (push (list key value) result))))
              (mapcar (lambda (x)
                        (cons (car x) (nreverse (cdr x))))
                      (nreverse result))))
           (lambda (a b) (> (car a) (car b)))))

(defun test-switcher-find-matching-file (file)
  "Compute the name of the test matching FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension file)))
         (candidates
          (cl-remove-if-not
           (lambda (current-file)
             (let ((name (file-name-nondirectory
                          (file-name-sans-extension current-file))))
               (or (some (lambda (prefix)
                           (string-equal name (concat prefix basename)))
                         test-prefixes)
                   (some (lambda (suffix)
                           (string-equal name (concat basename suffix)))
                         test-suffixes)
                   (some (lambda (prefix)
                           (string-equal basename (concat prefix name)))
                         test-prefixes)
                   (some (lambda (suffix)
                           (string-equal basename (concat name suffix)))
                         test-suffixes))))
           (funcall current-project-files-func))))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t (let ((grouped-candidates (test-switcher-group-file-candidates file candidates)))
          (if (= (length (car grouped-candidates)) 2)
              (car (last (car grouped-candidates)))
            (helm :sources
                  (helm-make-source "Projectile" 'helm-source-sync
                    :candidates (apply 'append (mapcar 'cdr grouped-candidates)))
                  :prompt "Switch to: "
                  :buffer "*helm-projectile*")))))))

(defun test-switcher-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename."
  (unless file-name (error "The current buffer is not visiting a file"))
  (let ((impl-file (test-switcher-find-matching-file file-name)))
    (if impl-file
        (expand-file-name impl-file (funcall test-switcher-project-root-func))
      (error "No matching source file found"))))

(defun test-switcher-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file
   (test-switcher-find-implementation-or-test (buffer-file-name))))

(provide 'test-switcher)
