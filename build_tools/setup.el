(require 'yaml)
(require 'json)
(require 'org)
(require 'ox)

(defvar-local pab/teaching-base-dir nil)
(defvar-local pab/teaching-export-dir nil)
(defvar-local pab/teaching-export-html-dir nil)
(defvar-local pab/teaching-export-tex-dir nil)
(defvar-local pab/teaching-export-dirs nil)
(defvar-local pab/teaching-site-dir nil)
(defvar-local pab/teaching-tex-dir nil)
(defvar-local pab/teaching-export-notes-dir nil)
(defvar-local pab/teaching-export-lectures-dir nil)
(defvar-local pab/teaching-export-problems-dir nil)


(defvar pab/teaching-mode-map (make-sparse-keymap))
(define-key pab/teaching-mode-map (kbd "C-c e") #'pab/teaching-export)
(define-key pab/teaching-mode-map (kbd "C-c x") #'pab/teaching-export-all)
(define-key pab/teaching-mode-map (kbd "C-c b") #'pab/teaching-create-export)

(define-minor-mode pab/teaching-mode
  "Minor mode for my teaching setup.

Loads key-maps and loads settings."

  :init-value nil
  :lighter " pab/teaching"

  (pab/teaching-load-settings)
  (pab/teaching-create-export))

(defun pab/teaching-load-settings ()
  "Load settings file."

  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-settings (json-read-file "settings.json")))

    (setq pab/teaching-base-dir
	  (expand-file-name (or (gethash "base_dir" json-settings)
				default-directory)))
    (setq pab/teaching-export-dir (expand-file-name (gethash "export_dir" json-settings) pab/teaching-base-dir))
    (setq pab/teaching-export-html-dir (expand-file-name "html" pab/teaching-export-dir))
    (setq pab/teaching-export-tex-dir (expand-file-name "tex" pab/teaching-export-dir))
    (setq pab/teaching-site-dir (expand-file-name (gethash "site_dir" json-settings)))
    (setq pab/teaching-tex-dir (expand-file-name (gethash "tex_dir" json-settings)))
    (setq pab/teaching-export-dirs (gethash "export_dirs" json-settings))
    (setq pab/teaching-export-notes-dir (gethash "notes" pab/teaching-export-dirs))
    (setq pab/teaching-export-lectures-dir (gethash "lectures" pab/teaching-export-dirs))
    (setq pab/teaching-export-problems-dir (gethash "problems" pab/teaching-export-dirs))))

(defun pab/teaching-create-export ()
  "Create export enviroment."
  (interactive)

  (make-directory pab/teaching-base-dir :parents)
  (make-directory pab/teaching-export-dir :parents)
  (make-directory pab/teaching-export-html-dir :parents)
  (make-directory pab/teaching-export-tex-dir :parents)
  (dolist (dir (list pab/teaching-export-html-dir pab/teaching-export-tex-dir))
    (maphash (lambda (hashkey hashval)
	       (make-directory (expand-file-name hashval dir) :parents))
	     pab/teaching-export-dirs))
  (copy-directory pab/teaching-site-dir pab/teaching-export-html-dir t t t)
  (copy-file (expand-file-name "macros.tex" pab/teaching-tex-dir) (expand-file-name "_includes/" pab/teaching-export-html-dir) t))

(defun pab/teaching-export-to-backend (outfile backends &optional post-process)
  "Export to OUTFILE at point using BACKENDS.

BACKENDS should be a list of backends to use.

For example ('html), ('html 'tex)

Currently implemented are html and tex.

Export to OUTFILE with extension appropriate for each backend in BACKEND.

If provided, POST-PROCESS should be an elisp function with one mandatory
argument containing a filename.
The function will be called after the org-export to post-process the result."

  (dolist (backend backends)
    (let (ext export-dir)
      (cond ((equal backend 'html)
	     (setq ext ".html")
	     (setq export-dir pab/teaching-export-html-dir))
	    ((equal backend 'latex)
	     (setq ext ".tex")
	     (setq export-dir pab/teaching-export-tex-dir)))
      (let ((filename
	     (file-name-with-extension (expand-file-name outfile export-dir) ext)))
	(make-directory (file-name-directory filename) t)
	(org-export-to-file backend filename nil t nil t nil post-process)))))

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
Otherwise return FILENAME_PREFIX-CUSTOM_ID
where CUSTOM_ID is obtained from the tags."

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

(defun pab/teaching-export-subnote (notename)
  "Export sub-topic at point to file determined by NOTENAME.

The name of the file is obtained by calling
pab/teaching-export-subtopic-file-name.

The exported file well be created in

pab/teaching-export-notes-dir/notename."

  (when (pab/teaching-subnote-p)
    (let* ((export-filename
	    (file-name-concat pab/teaching-export-notes-dir
			      notename
			      (pab/teaching-export-subtopic-file-name notename))))
      (pab/teaching-export-to-backend export-filename '(html) #'pab/teaching-export-subnote-post-process))))

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

(defun pab/teaching-export-note (notename)
  "Export note at point to file determined by NOTENAME.

The name of the file is obtained by calling
pab/teaching-export-note-file-name.

The note is exported to both html and LaTeX along with
all subnotes..

The exported html file is created as

pab/teaching-export-notes-dir/NOTENAME/index.html

The exported LaTeX file is created as

pab/teaching-export-notes-tex-dir/NOTENAME

The subnotes are exported by calling
pab/teaching-export-subnote on each subnote."

  (when (pab/teaching-note-p)
      (let*
	  ((hash (pab/teaching-note-hash-frontmatter))
	   (note-level (org-element-property :level (org-element-at-point)))
	   (outfile (file-name-concat pab/teaching-export-html-dir pab/teaching-export-notes-dir notename "index.html"))
	   (texfile (file-name-concat pab/teaching-export-notes-dir notename)))
	(make-directory (file-name-directory outfile) t)
	(with-temp-buffer
	  (write-region nil nil outfile))
	(pab/teaching-prepend-hash-to-file-as-yaml-frontmatter outfile hash)

	(pab/teaching-export-to-backend texfile '(latex))
	(org-map-entries
	 (lambda() (pab/teaching-export-subnote notename)) (format "LEVEL=%d" (+ note-level 1)) 'tree))))

(defun pab/teaching-export-lecture (filename)
  "Export a lecture to FILENAME.

The output is in pab/teaching-export-lectures-dir."

  (when (pab/teaching-lecture-p)
	(pab/teaching-export-to-backend (file-name-concat pab/teaching-export-lectures-dir filename) '(html))))

(defun pab/teaching-export-problems (filename)
  "Export a problem to FILENAME.

The output is in pab/teaching-export-problems-dir."

  (when (pab/teaching-problem-p)
    (pab/teaching-export-to-backend (file-name-concat pab/teaching-export-problems-dir filename) '(html latex))))

(defun pab/teaching-export-challenge (filename)
  "Export a challenge to FILENAME.

The output is in pab/teaching-export-problems-dir."

  (when (pab/teaching-challenge-p)
    (pab/teaching-export-to-backend (file-name-concat pab/teaching-export-problems-dir filename) '(html latex))))

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
	   (pab/teaching-export-lecture (format "lectures_%s" filename)))
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
