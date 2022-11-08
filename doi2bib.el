;;; doi2bib.el --- DOI to bibtex -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 John Kitchin
;; Copyright (C) 2022 Akira Kyle

;; Author: Akira Kyle <akira@akirakyle.com>
;; URL: https://github.com/akirakyle/emacs-webkit
;; Version: 0.1
;; Package-Requires: ((dash))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; See README.org

;;; Code:

(defvar url-http-end-of-headers)
(require 'url-http)
(require 'bibtex)
(require 'json)

(require 'dash)

(setq doi2bib-bibliography nil);'citar-bibliography)
(setq doi2bib-dx-doi-org-url "https://doi.org/")
;; "Base url to retrieve doi metadata from. A trailing / is required."
;; Could also use "http://dx.doi.org/"?

;; Getting bibtex entries from a DOI
(defun doi2bib-get-json-metadata (doi)
  "Get json metadata for DOI."
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json")
        (json-object-type 'plist)
        (json-data)
	(url (concat doi2bib-dx-doi-org-url doi)))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (setq json-data (buffer-substring url-http-end-of-headers (point-max)))
      (when (or (string-match "<title>Error: DOI Not Found</title>" json-data)
		(string-match "Resource not found" json-data)
		(string-match "Status *406" json-data)
		(string-match "400 Bad Request" json-data))
	(error "Something went wrong.  We got this response:
%s" json-data))
      (json-read-from-string json-data))))

;; We can use that data to construct a bibtex entry. We do that by defining a
;; template, and filling it in. I wrote this template expansion code which
;; makes it easy to substitute values like %{} in emacs lisp.
(defun doi2bib-expand-template (s)
  "Expand a string template S containing %{} with the eval of its contents."
  (replace-regexp-in-string "%{\\([^}]+\\)}"
			    (lambda (arg)
			      (let ((sexp (substring arg 2 -1)))
				(format "%s" (eval (read sexp)))))
			    s))


;; Now we define a function that fills in that template from the metadata.

;; As different bibtex types share common keys, it is advantageous to separate
;; data extraction from json, and the formatting of the bibtex entry.

;; We use eval-and-compile because we use the three following forms in the
;; `doi2bib-def-bibtex-type' macro.  Since the macro is expanded at compile
;; time, we need to ensure these defuns and defvars are evaluated at
;; compile-time.
(eval-and-compile
  (defvar doi2bib-json-metadata-extract
    '((type       (plist-get results :type))
      (author     (mapconcat (lambda (x)
			       (message "%s" x)
			       (if (plist-get x :name)
				   (plist-get x :name)
				 (concat (plist-get x :given) " " (plist-get x :family))))
                             (plist-get results :author) " and "))
      (title      (plist-get results :title))
      (subtitle   (plist-get results :subtitle))
      (journal    (plist-get results :container-title))
      (series     (plist-get results :container-title))
      (publisher  (plist-get results :publisher))
      (volume     (plist-get results :volume))
      (issue      (plist-get results :issue))
      (number     (plist-get results :issue))
      (year       (or (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0)
                      (elt (elt (plist-get (plist-get results :approved) :date-parts) 0) 0)
                      ))
      ;; Some dates don't have a month in them.
      (month      (let ((date (elt
			       (plist-get (plist-get results :issued) :date-parts) 0)))
		    (if (>= (length date) 2)
			(elt date 1)
		      "-")))
      (pages      (or (plist-get results :page)
		      (plist-get results :article-number)))
      (doi        (plist-get results :DOI))
      (url        (plist-get results :URL))
      (booktitle  (plist-get results :container-title))
      (school     (or (plist-get results :school)
                      (plist-get (plist-get results :institution) :name)))))

  ;; Next, we need to define the different bibtex types. Each type has a bibtex
  ;; type (for output) and the type as provided in the doi record. Finally, we
  ;; have to declare the fields we want to output.
  (defvar doi2bib-bibtex-type-generators nil)

  (defun doi2bib-concat-reduce (e1 e2)
    (if (and (stringp (car (last e1))) (stringp (car e2)))
        (append (butlast e1) (list (concat (car (last e1)) (car e2))) (cdr e2))
      (append e1 e2))))

(defmacro doi2bib-def-bibtex-type (name matching-types &rest fields)
  "Define a BibTeX type identified by (symbol) NAME.
MATCHING-TYPES is a list of strings.  FIELDS are symbols that
match to retrieval expressions in
`doi2bib-json-metadata-extract'.  This type will only be used
when the `:type' parameter in the JSON metadata is contained in
MATCHING-TYPES."
  `(push (lambda (type results)
           (when
               (or ,@(mapcar
                      (lambda (match-type)
                        `(string= type ,match-type)) matching-types))
             (let ,(mapcar (lambda (field)
                             (let ((field-expr
                                    (assoc field doi2bib-json-metadata-extract)))
                               (if field-expr
                                   ;; need to convert to string first
                                   `(,(car field-expr) (format "%s" ,(cadr field-expr)))
                                 (error "Unknown bibtex field type %s" field))))
                           fields)
               (concat ,@(seq-reduce
                          'doi2bib-concat-reduce
                          (seq-map
                           'list 
                           (flatten-tree
                            (list "@" (symbol-name name) "{,\n"
                                  (seq-map
                                   (lambda (field)
                                     `("  " ,(symbol-name field) " = {" ,field "},\n"))
                                   fields)
                                  "}\n"))) nil)))))
         doi2bib-bibtex-type-generators))

(doi2bib-def-bibtex-type article ("journal-article" "article-journal" "article")
                           author title journal year volume number month pages doi url)

(doi2bib-def-bibtex-type inproceedings ("proceedings-article" "paper-conference")
                           author title booktitle year month pages doi url)

(doi2bib-def-bibtex-type book ("book")
                           author title series publisher year pages doi url)

(doi2bib-def-bibtex-type inbook ("chapter" "book-chapter" "reference-entry")
                           author title booktitle series publisher year pages doi url)

(doi2bib-def-bibtex-type phdthesis ("phdthesis" "thesis" "dissertation")
                  author title school publisher year)

(doi2bib-def-bibtex-type misc ("posted-content")
			   author title year doi url publisher)


;; With the code generating the bibtex entry in place, we can glue it to the json retrieval code.
(defun doi2bib-doi-to-bibtex-string (doi)
  "Return a bibtex entry as a string for the DOI.  Not all types are supported yet."
  (let* ((results (doi2bib-get-json-metadata doi))
         (type
          (if (string= "arXiv" (plist-get results :publisher))
              "posted-content"
            (plist-get results :type))))
    ;(message (format "%s" results))
    (or (-some (lambda (g) (funcall g type results)) doi2bib-bibtex-type-generators)
        (message "%s not supported yet\n%S." type results))))

;; That is just the string for the entry. To be useful, we need a function that
;; inserts the string into a buffer. This function will insert the string at the
;; cursor, clean the entry.
(defun doi2bib-insert-bibtex-entry-from-doi (doi)
  "Insert and clean bibtex entry from a DOI."
  (insert (doi2bib-doi-to-bibtex-string doi))
  (backward-char)
  ;(bibtex-autokey-edit-before-use nil)
  (bibtex-clean-entry t)
  (save-buffer))


;;;###autoload
(defun doi2bib-add-bibtex-entry-from-doi (doi &optional bibfile)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi2bib-maybe-doi-from-region-or-current-kill))))

  (unless bibfile
    (setq bibfile (completing-read "Bibfile: " doi2bib-bibliography)))
  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (re-search-forward (concat doi "\\_>") nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))

	(when (not (looking-back "\n\n" (min 3 (point))))
	  (insert "\n\n"))

        (doi2bib-insert-bibtex-entry-from-doi doi)
        (save-buffer)))))

(defalias 'doi-add-bibtex-entry 'doi2bib-add-bibtex-entry-from-doi
  "Alias function for convenience.")

(defun doi2bib-maybe-doi-from-region-or-current-kill ()
  "Try to get a DOI from the active region or current kill."
  (let* ((the-active-region (if (region-active-p) ;; nil if no active region
                                (buffer-substring (region-beginning) (region-end))
                              nil))
         (the-current-kill (ignore-errors (current-kill 0 t)))  ;; nil if empty kill ring
         ;; DOI urls
         ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
         ;; Ex: https://dx.doi.org/10.1007/978-3-319-60876-1_10
         (doi-url-prefix-regexp "^https?://\\(dx\\.\\)?doi\\.org/")
         ;; https://www.crossref.org/blog/dois-and-matching-regular-expressions/
         (doi-regexp "10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+$"))
    (cond
     ;; Check if a DOI can be found in the active region
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat "^" doi-regexp) the-active-region))
      the-active-region)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-active-region))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-active-region))
     ;; DOI url as customized
     ((and (stringp the-active-region)
           (s-match (regexp-quote doi2bib-dx-doi-org-url) the-active-region))
      (replace-regexp-in-string (regexp-quote doi2bib-dx-doi-org-url) "" the-active-region))
     ;; Check if DOI can be found in the current kill
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat "^" doi-regexp) the-current-kill))
      the-current-kill)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-current-kill))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-current-kill))
     ;; DOI url as customized
     ((and (stringp the-current-kill)
           (s-match (regexp-quote doi2bib-dx-doi-org-url) the-current-kill))
      (replace-regexp-in-string (regexp-quote doi2bib-dx-doi-org-url) "" the-current-kill))
     ;; otherwise, return nil
     (t
      nil))))

(provide 'doi2bib)
;;; doi2bib.el ends here
