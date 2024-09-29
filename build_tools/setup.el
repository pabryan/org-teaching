(require 'yaml)
(require 'json)
(require 'org)
(require 'ox)

(defvar-local pab/teaching-build-dir nil)
(defvar-local pab/teaching-export-dir nil)
(defvar-local pab/teaching-publish-dir nil)
(defvar-local pab/teaching-publish-dirs nil)
(defvar-local pab/teaching-site-dir nil)

(defvar pab/teaching-mode-map (make-sparse-keymap))
(define-key pab/teaching-mode-map (kbd "C-c e") #'pab/teaching-export)
(define-key pab/teaching-mode-map (kbd "C-c x") #'pab/teaching-export-all)
(define-key pab/teaching-mode-map (kbd "C-c h") (lambda () (pab/teaching-export-to-backend 'html)))
(define-key pab/teaching-mode-map (kbd "C-c t") (lambda () (pab/teaching-export-to-backend 'latex)))

(define-minor-mode pab/teaching-mode
  "Minor mode for my teaching setup.

Loads key-maps and loads settings."

  :init-value nil
  :lighter " pab/teaching"

  (pab/teaching-load-settings)
  (pab/teaching-create-build))

(defun pab/teaching-load-settings ()
  "Load settings file."

  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-settings (json-read-file "settings.json")))

    (setq pab/teaching-build-dir (expand-file-name (gethash "build_dir" json-settings)))
    (setq pab/teaching-export-dir (expand-file-name (gethash "export_dir" json-settings) pab/teaching-build-dir))
    (setq pab/teaching-publish-dir (expand-file-name (gethash "publish_dir" json-settings) pab/teaching-build-dir))
    (setq pab/teaching-site-dir (gethash "site_dir" json-settings))
    (setq pab/teaching-publish-dirs (gethash "publish_dirs" json-settings))
    (maphash
     (lambda (hashkey hashval)
       (puthash hashkey (expand-file-name hashval pab/teaching-publish-dir) pab/teaching-publish-dirs)) pab/teaching-publish-dirs)))

(defun pab/teaching-create-build ()
  "Create build enviroment."
  (interactive)

  (make-directory pab/teaching-build-dir :parents)
  (make-directory pab/teaching-export-dir :parents)
  (make-directory pab/teaching-publish-dir :parents)
  (maphash (lambda (hashkey hashval)
	     (make-directory hashval :parents))
	   pab/teaching-publish-dirs)

(defun pab/teaching-export-to-backend (backends &optional outfile post-process)
  "Export at point using BACKENDS.
BACKENDS should be a list of backends to use.

For example ('html), ('html 'tex)

Currently implemented are html and tex.

If OUTFILE is set, export to that file in the pab/teaching-export-dir directory
with appropriate file extension for echo backend in BACKENDS
Otherwise let org decide the filename

If provided, POST-PROCESS should be an elisp function with one mandatory
argument containing a filename.
The function will be called after the org-export to post-process the result."


  (dolist (backend backends)
    (let ((ext (cond ((equal backend 'html) ".html")
		     ((equal backend 'latex) ".tex")))
	  (filename nil))
      (if outfile
	  (setq filename (file-name-concat pab/teaching-export-dir (file-name-with-extension outfile ext)))
	(setq filename (org-export-output-file-name ext t pab/teaching-export-dir)))
      (org-export-to-file backend filename nil t nil t nil post-process))))

(defun pab/teaching-prepend-hash-to-file-as-yaml-frontmatter (filename hash)
  "Prepend HASH to FILENAME as yaml-frontmatter.

HASH should be a hash to be converted to yaml frontmatter.
FILENAME should be a string containing the name of a file.

The result is of the form

---
<yaml>
---

<filecontents>"

  (with-temp-buffer
      (insert (pab/teaching-hash-to-yaml-frontmatter hash))
      (insert-file-contents filename)
      (write-region nil nil filename)))

(defun pab/teaching-hash-to-yaml-frontmatter (hash)
  "Convert HASH into a string of yaml frontmatter."

  (format "---\n%s\n---\n\n" (yaml-encode hash)))

(defun pab/teaching-export-file-name ()
  "Construct the file-name for export.
returns nil if EXPORT_FILE_NAME is already set
otherwise builds filename as <week>_<lec>_<name>"

  (unless (org-entry-get nil "EXPORT_FILE_NAME")
    (let ((name (org-entry-get-with-inheritance "NAME"))
	  (week (org-entry-get-with-inheritance "WEEK"))
	  (lec (org-entry-get-with-inheritance "LECTURE")))
      (format "%s_%s_%s" week lec name))))

(defun pab/teaching-export-subtopic-file-name (filename_prefix)
  "Construct the file-name for subtopic export.
Accepts one argument FILENAME_PREFIX

Returns nil if EXPORT_FILE_NAME is already set.
Otherwise return FILENAME-CUSTOM_ID where CUSTOM_ID is obtained from the tags."

  (unless (org-entry-get nil "EXPORT_FILE_NAME")
    (let ((topic (org-entry-get-with-inheritance "CUSTOM_ID")))
      (format "%s-%s" filename_prefix topic))))

(defun pab/teaching-subnote-p ()
  "Test if headline at point is a sub-note.

This will be true provided the headline one level
higher is a note."

  (save-excursion
    (org-up-heading-safe)
    (pab/teaching-note-p)))

(defun pab/teaching-note-p ()
  "Test if headline at point is a note."

  (member "notes" (org-get-tags nil t)))

(defun pab/teaching-lecture-p ()
  "Test if headline at point is a lecture."

  (member "lecture" (org-get-tags nil t)))

(defun pab/teaching-problem-p ()
  "Test if headline at point is a problem."

  (member "problems" (org-get-tags nil t)))

(defun pab/teaching-challenge-p ()
  "Test if headline at point is a challenge."

  (member "challenge" (org-get-tags nil t)))

(defun pab/teaching-export-subnote (&optional filename)
  "Export sub-topics.

Saves to FILENAME if passed, otherwise let org decide the filename."

  (when (pab/teaching-subnote-p)
      (let ((export-filename (pab/teaching-export-subtopic-file-name filename)))
	(pab/teaching-export-to-backend '(html) export-filename #'pab/teaching-export-subnote-post-process))))

(defun pab/teaching-subnote-hash-frontmatter ()
  "Generate hash frontmatter for subnote."

  (let*
      ((pagename (org-entry-get-with-inheritance "NAME"))
	(week (org-entry-get-with-inheritance "WEEK"))
	(lec (org-entry-get-with-inheritance "LECTURE"))
	(id (org-entry-get-with-inheritance "CUSTOM_ID"))
	(title (org-element-property :title (org-element-at-point))))
    (list (cons 'pagename (format "%s-%s" pagename id))
		    (cons 'week week)
		    (cons 'lec lec)
		    (cons 'title title))))

(defun pab/teaching-export-subnote-post-process (filename)
  "Post process a subnote after org-export-to-file.

FILENAME is the name of the file exported by org.

Adds yaml frontmatter to subnote.
Then runs python post-processing script."

  (pab/teaching-prepend-hash-to-file-as-yaml-frontmatter
   filename (pab/teaching-subnote-hash-frontmatter))
  (shell-command (format "./build_tools/post_process.py -t s %1$s %1$s" filename)))

(defun pab/teaching-get-notes-topics ()
  "Get list of topics under current note."

  (when (pab/teaching-note-p)
      (let ((note-level (org-element-property :level (org-element-at-point))))
	(org-map-entries
	 (lambda()
	   (org-entry-get nil "CUSTOM_ID"))
	 (format "LEVEL=%d" (+ note-level 1)) 'tree))))

(defun pab/teaching-note-hash-frontmatter ()
  "Generate hash frontmatter for note."

  (let*
      ((layout "note")
       (pagename (org-entry-get-with-inheritance "NAME"))
       (title (org-entry-get-with-inheritance "TITLE"))
       (week (org-entry-get-with-inheritance "WEEK"))
       (lec (org-entry-get-with-inheritance "LECTURE"))
       (abstract (pab/teaching-note-abstract)))
    (list
     (cons 'layout layout)
     (cons 'pagename pagename)
     (cons 'title title)
     (cons 'week week)
     (cons 'lec lec)
     (cons 'topics (pab/teaching-get-notes-topics))
     (cons 'abstract (format "%s" abstract)))))

(defun pab/teaching-note-abstract ()
  "Return the first paragraph of the first subnote."

  (save-excursion
    (org-goto-first-child)
    (let ((el (org-element-at-point)))
      (pab/teaching-first-para el))))

(defun pab/teaching-first-para (element)
  "Message the first paragraph following headline ELEMENT, if present.
When called interactively, ELEMENT is the element at point."

  (save-excursion
    (when-let ((beg (org-element-property :contents-begin element)))
      (goto-char beg)
      (let* ((el (org-element-at-point)))
	(when (eq 'property-drawer (org-element-type el))
	  (setq el (pab/teaching-org-next-element el)))
	(when (eq 'paragraph (org-element-type el))
	  (buffer-substring-no-properties
           (org-element-property :contents-begin el)
           (org-element-property :contents-end el)))))))

(defun pab/teaching-org-next-element (element)
  "Return the next element after ELEMENT."

  (save-excursion
    (let ((end (org-element-property :end element)))
      (goto-char (+ end 1))
      (org-element-at-point))))

(defun pab/teaching-export-note (&optional filename)
  "Export a note.

Saves to FILENAME if passed, otherwise let org decide the filename."

  (when (pab/teaching-note-p)
      (let*
	  ((hash (pab/teaching-note-hash-frontmatter))
	   (note-level (org-element-property :level (org-element-at-point)))
	   (outfile (file-name-concat pab/teaching-export-dir (format "%s.%s" filename "html"))))
	(with-temp-buffer
	  (write-region nil nil outfile))
	(pab/teaching-prepend-hash-to-file-as-yaml-frontmatter outfile hash)
	(pab/teaching-export-to-backend '(latex) filename)
	(org-map-entries
	 (lambda() (pab/teaching-export-subnote filename)) (format "LEVEL=%d" (+ note-level 1)) 'tree))))

(defun pab/teaching-export-lecture (&optional filename)
  "Export a lecture.

Saves to FILENAME if passed, otherwise let org decide the filename."

  (when (pab/teaching-lecture-p)
	(pab/teaching-export-to-backend '(html) filename)))

(defun pab/teaching-export-problems (&optional filename)
  "Export problems.

Saves to FILENAME if passed, otherwise let org decide the filename."

  (when (pab/teaching-problem-p)
      (pab/teaching-export-to-backend '(html latex) filename)))

(defun pab/teaching-export-challenge (&optional filename)
  "Export challenge.

Saves to FILENAME if passed, otherwise let org decide the filename."

  (when (pab/teaching-challenge-p)
	(pab/teaching-export-to-backend '(html latex) filename)))

(defun pab/teaching-export ()
  "Export headline based on tag.

Possible tags are 'notes', 'lecture', 'problems', 'challenge'"

  (interactive)
  (let ((filename (pab/teaching-export-file-name)))
    (cond ((pab/teaching-note-p)
	   (pab/teaching-export-note (format "notes_%s" filename)))
	  ((pab/teaching-subnote-p)
	   (pab/teaching-export-subnote (format "notes_%s" filename)))
	  ((pab/teaching-lecture-p)
	   (pab/teaching-export-lecture (format "lecture_%s" filename)))
	  ((pab/teaching-problem-p)
	   (pab/teaching-export-problems (format "problems_%s"filename)))
	  ((pab/teaching-challenge-p)
	   (pab/teaching-export-challenge (format "challenge_%s"filename)))
	  ('t (message "Can't export from here")))))

(defun pab/teaching-export-all ()
  "Export all notes, lectures and problems/challenges."

  (interactive)
  (let ((org-use-tag-inheritance nil))
    (org-map-entries #'pab/teaching-export "+notes-noexport|+lecture-noexport|+problems-noexport|+challenge-noexport")))
