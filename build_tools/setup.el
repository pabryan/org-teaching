(require 'yaml)
(require 'json)
(require 'org)
(require 'ox)

(defvar-local pab/teaching-base-dir nil)
(defvar-local pab/teaching-export-dir nil)
(defvar-local pab/teaching-export-html-dir nil)
(defvar-local pab/teaching-export-tex-dir nil)
(defvar-local pab/teaching-export-html-figs-dir nil)
(defvar-local pab/teaching-export-tex-figs-dir nil)
(defvar-local pab/teaching-export-dirs nil)
(defvar-local pab/teaching-site-dir nil)
(defvar-local pab/teaching-tex-dir nil)
(defvar-local pab/teaching-export-notes-dir nil)
(defvar-local pab/teaching-export-lectures-dir nil)
(defvar-local pab/teaching-export-problems-dir nil)
(defvar-local pab/teaching-build_tools-dir nil)

(defvar-local pab/teaching-mode-map (make-sparse-keymap))
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
    (setq pab/teaching-export-html-dir (expand-file-name (gethash "html_dir" json-settings) pab/teaching-export-dir))
    (setq pab/teaching-export-tex-dir (expand-file-name (gethash "tex_dir" json-settings) pab/teaching-export-dir))
    (let ((figs-dir (gethash "figs_dir" json-settings)))
      (setq pab/teaching-export-html-figs-dir (expand-file-name figs-dir pab/teaching-export-html-dir))
      (setq pab/teaching-export-tex-figs-dir (expand-file-name figs-dir pab/teaching-export-tex-dir)))
    (setq pab/teaching-site-dir (expand-file-name (gethash "site_dir" json-settings)))
    (setq pab/teaching-tex-dir (expand-file-name (gethash "tex_dir" json-settings)))
    (setq pab/teaching-export-dirs (gethash "export_dirs" json-settings))
    (setq pab/teaching-export-notes-dir (gethash "notes" pab/teaching-export-dirs))
    (setq pab/teaching-export-lectures-dir (gethash "lectures" pab/teaching-export-dirs))
    (setq pab/teaching-export-problems-dir (gethash "problems" pab/teaching-export-dirs))
    (setq pab/teaching-build_tools-dir (expand-file-name (gethash "build_tools_dir" json-settings) pab/teaching-base-dir))))

(defun pab/teaching-create-export ()
  "Create export enviroment."
  (interactive)

  (make-directory pab/teaching-base-dir :parents)
  (make-directory pab/teaching-export-dir :parents)
  (make-directory pab/teaching-export-html-dir :parents)
  (make-directory pab/teaching-export-tex-dir :parents)
  (make-directory pab/teaching-export-html-figs-dir :parents)
  (make-directory pab/teaching-export-tex-figs-dir :parents)
  (dolist (dir (list pab/teaching-export-html-dir pab/teaching-export-tex-dir))
    (maphash (lambda (hashkey hashval)
	       (make-directory (expand-file-name hashval dir) :parents))
	     pab/teaching-export-dirs))
  (copy-directory pab/teaching-site-dir pab/teaching-export-html-dir t t t)
  (copy-file (expand-file-name "macros.tex" pab/teaching-tex-dir) (expand-file-name "_includes/" pab/teaching-export-html-dir) t)
  (copy-directory pab/teaching-tex-dir pab/teaching-export-tex-dir t t t))

(defun pab/teaching-export-to-backend (outfile backends &optional post-process)
  "Export to OUTFILE at point using BACKENDS.

BACKENDS should be a list of backends to use.

For example (\\='html), (\\='html \\='tex)

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
  (shell-command (format "%1$s -t s %2$s %2$s"
			 (expand-file-name "post_process.py" pab/teaching-build_tools-dir)
			 filename)))

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
      (pab/teaching-export-note-html notename)
      (pab/teaching-export-note-pdf notename)))

(defun pab/teaching-export-note-html (notename)
  "Export note NOTENAME to html."

  (let
      ((hash (pab/teaching-note-hash-frontmatter))
       (outfile (file-name-concat pab/teaching-export-html-dir pab/teaching-export-notes-dir notename "index.html")))
    (make-directory (file-name-directory outfile) t)
    (with-temp-buffer
      (write-region nil nil outfile))
    (pab/teaching-prepend-hash-to-file-as-yaml-frontmatter outfile hash))

  (let ((note-level (org-element-property :level (org-element-at-point))))
    (org-map-entries
     (lambda() (pab/teaching-export-subnote notename)) (format "LEVEL=%d" (+ note-level 1)) 'tree)))

(defun pab/teaching-export-note-pdf (notename)
  "Export note NOTENAME to pdf."

  (let ((texfile (file-name-concat pab/teaching-export-notes-dir notename))
	(latex-cmd (format "%s %s %s %s"
			   (expand-file-name "compile_note.sh" pab/teaching-build_tools-dir)
			   notename
			   pab/teaching-export-notes-dir
			   (expand-file-name "notes_template.tex" pab/teaching-export-tex-dir))))
    (pab/teaching-export-to-backend texfile '(latex))
    (pab/teaching-make-note-tex-spec notename)
    (shell-command latex-cmd)))

(defun pab/teaching-make-note-tex-spec (notename)
  "Create a note tex spec file for NOTENAME."

  (let ((specfile
	(expand-file-name (format "%s-spec.tex" notename)
			  (file-name-concat pab/teaching-export-tex-dir pab/teaching-export-notes-dir))))
    (with-temp-buffer
      (insert (format "\\newcommand{\\weeknum}{%s}\n" "01"))
      (insert (format "\\newcommand{\\topic}{%s}\n" "Topic"))
      (insert (format "\\newcommand{\\src}{%s}\n" notename))
      (write-region nil nil specfile))))

(defun pab/teaching-lecture-hash-frontmatter ()
  "Generate hash frontmatter for lecture."

  (let*
      ((layout "slides")
       (pagename (org-entry-get-with-inheritance "NAME"))
       (title (org-entry-get-with-inheritance "TITLE"))
       (week (org-entry-get-with-inheritance "WEEK"))
       (lec (org-entry-get-with-inheritance "LECTURE")))
    (list
     (cons 'layout layout)
     (cons 'pagename pagename)
     (cons 'title title)
     (cons 'week week)
     (cons 'lec lec))))

(defun pab/teaching-export-lecture-post-process (filename)
  "Post process a lecture after org-export-to-file.

FILENAME is the name of the file exported by org.

Adds yaml frontmatter to lecture.
Then runs python post-processing script."

  (pab/teaching-prepend-hash-to-file-as-yaml-frontmatter
   filename (pab/teaching-lecture-hash-frontmatter))
  (shell-command (format "%1$s -t l %2$s %2$s"
			 (expand-file-name "post_process.py" pab/teaching-build_tools-dir)
			 filename)))

(defun pab/teaching-export-lecture (filename)
  "Export a lecture to FILENAME.

The output is in pab/teaching-export-lectures-dir."

  (when (pab/teaching-lecture-p)
	(pab/teaching-export-to-backend (file-name-concat pab/teaching-export-lectures-dir filename) '(html) #'pab/teaching-export-lecture-post-process)))

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

(defun pab/teaching-export (&optional nobuildp)
  "Export headline based on tag.

If NOBUILDP is nil (the default), then build the export environment first
If NOBUILDP is non-nil, then don't build the export environment.

Possible tags are notes, lecture, problems, challenge"

  (interactive "P")
  (unless nobuildp
    (pab/teaching-create-export))

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
  (pab/teaching-create-export)
  (let ((org-use-tag-inheritance nil))
    (org-map-entries (lambda () (pab/teaching-export t)) "+notes-noexport|+lecture-noexport|+problems-noexport|+challenge-noexport")))

(defun pab/org-export-figure-markup (filename caption)
  "On org export, output backend dependent markup for figure in FILENAME.

The markup is given a caption of CAPTION.

For html the export is a Jekyll include for an interactive  plotly graph.

For latex it's a png."

  (let* ((export org-export-current-backend)
	 (format-string
	  (cond
	   ((eq 'html export) "{%% include plotly.html filename=\\\"%1$s\\\" caption=\\\"%1$s\\\" %%}")
	   ((eq 'latex export) "#+CAPTION: %2$s\\n[[file:%1$s.png]]")
	   (t "[[file:%1$s.png]]"))))
    (format format-string filename caption)))
