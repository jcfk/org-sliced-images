;;; org-sliced-images.el --- Sliced inline images in org-mode. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;;
;; Package-Requires: ((org))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This package ports insert-sliced-image functionality into org-mode, with
;; respect to inline image display. This is supposed to make scrolling over
;; large images more visually pleasing. The package overrides a number of
;; function of the org-mode image display.
;;
;; Usage
;; -----
;;
;; Usage is identical to that of vanilla org, aside from the following
;; customizations.
;;
;; Regarding dummy lines
;; ---------------------
;;
;; Dummy lines are added to support slice overlays.  Lines matching dummy lines
;; coming directly after a link can be overlaid, instead of adding new ones,
;; depending on the value of `org-sliced-images-consume-dummies'.  This prevents
;; a massive buildup of dummy lines over multiple file saves.
;;
;;; Code:

(require 'org)

;; Configuration

(defgroup org-sliced-images nil
  "Configure org-sliced-images."
  :group 'org)

(defcustom org-sliced-images-consume-dummies t
  "Overlay existing dummy lines instead of adding new ones."
  :type 'boolean
  :group 'org-sliced-images)

;; Buffer variables

(defvar-local org-inline-image-overlay-families nil
  "A list of elements corresponding to displayed inline images in the
current buffer. Each element is a list of overlays making up the
displayed image.
The first element in each list is an overlay over the dummy lines
inserted to support the slices. The remaining elements are the slices
themselves; the last element is the topmost slice.")
(put 'org-inline-image-overlay-families 'permanent-local t)

;; Function overrides

;;;###autoload
(defun org--delete-inline-image-overlay-family (ovfam)
  "Delete the overlay family OVFAM from its buffer."
  (dolist (ov (cdr ovfam))
    (delete-overlay ov))
  (delete-region (overlay-start (car ovfam)) (1+ (overlay-end (car ovfam)))) ;; 1+??
  (delete-overlay (car ovfam)))

;;;###autoload
(defun org--inline-image-overlay-families (&optional beg end)
  "Return image overlay families which start between BEG and END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end))
         result)
    (dolist (ovfam org-inline-image-overlay-families result)
      (when (memq (car (last ovfam)) overlays)
        (push ovfam result)))))

;;;###autoload
(defun org-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if (org--inline-image-overlay-families beg end)
      (progn
        (org-remove-inline-images beg end)
        (when (called-interactively-p 'interactive)
	      (message "Inline image display turned off")))
    (org-display-inline-images include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org--inline-image-overlay-families beg end)))
        (message (if new
		             (format "%d images displayed inline"
			                 (length new))
		           "No images to display inline"))))))

;;;###autoload
(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    (`cache (let ((revert-without-query '(".")))
		      (with-current-buffer (find-file-noselect file)
			(buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width :scale 1 :ascent 'center))))

;;;###autoload
(defun org--make-inline-image-overlay (start end spec)
  "Make overlay from START to END with display value SPEC.
The overlay is returned."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'display spec)
    (overlay-put ov 'face 'default)
    (overlay-put ov 'org-image-overlay t)
    (overlay-put ov 'modification-hooks
                 (list 'org-display-inline-remove-overlay-family))
    (when (boundp 'image-map)
      (overlay-put ov 'keymap image-map))
    ov))

;;;###autoload
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	    (let* ((case-fold-search t)
	           (file-extension-re (image-file-name-regexp))
	           (link-abbrevs (mapcar #'car
				                     (append org-link-abbrev-alist-local
					                         org-link-abbrev-alist)))
	           ;; Check absolute, relative file names and explicit
	           ;; "file:" links.  Also check link abbreviations since
	           ;; some might expand to "file" links.
	           (file-types-re
		        (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			            (if (not link-abbrevs) ""
			              (concat "\\|" (regexp-opt link-abbrevs))))))
	      (while (re-search-forward file-types-re end t)
	        (let* ((link (org-element-lineage
			              (save-match-data (org-element-context))
			              '(link) t))
                   (linktype (org-element-property :type link))
		           (inner-start (match-beginning 1))
		           (path
		            (cond
		             ;; No link at point; no inline image.
		             ((not link) nil)
		             ;; File link without a description.  Also handle
		             ;; INCLUDE-LINKED here since it should have
		             ;; precedence over the next case.  I.e., if link
		             ;; contains filenames in both the path and the
		             ;; description, prioritize the path only when
		             ;; INCLUDE-LINKED is non-nil.
		             ((or (not (org-element-property :contents-begin link))
			              include-linked)
		              (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			               (org-element-property :path link)))
		             ;; Link with a description.  Check if description
		             ;; is a filename.  Even if Org doesn't have syntax
		             ;; for those -- clickable image -- constructs, fake
		             ;; them, as in `org-export-insert-image-links'.
		             ((not inner-start) nil)
		             (t
		              (org-with-point-at inner-start
			            (and (looking-at
			                  (if (char-equal ?< (char-after inner-start))
				                  org-link-angle-re
				                org-link-plain-re))
			                 ;; File name must fill the whole
			                 ;; description.
			                 (= (org-element-property :contents-end link)
				                (match-end 0))
			                 (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
	          (when (and path (string-match-p file-extension-re path))
		        (let ((file (if (equal "attachment" linktype)
				                (progn
                                  (require 'org-attach)
				                  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
		          (when (and file (file-exists-p file))
		            (let ((width (org-display-inline-image--width link))
			              (old (get-char-property-and-overlay
				                (org-element-property :begin link)
				                'org-image-overlay)))
		              (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
			            (let ((image (org--create-inline-image file width)))
			              (when image
                            (let* ((image-pixel-cons (image-size image t))
                                   (image-pixel-h (cdr image-pixel-cons))
                                   (image-line-h (/ image-pixel-h (default-font-height) 1.0))
                                   (y 0.0) (dy (/ image-line-h))
                                   (dummy-zone-start nil)
                                   (dummy-zone-end nil)
                                   (ovfam nil))
                              (image-flush image)
                              (org-with-point-at (org-element-property :begin link)
                                (while (< y 1.0)
                                  (let (start end)
                                    (if (= y 0.0)
                                        ;; Overlay link
                                        (setq start (org-element-property :begin link)
                                              end (org-element-property :end link))
                                      (if (and org-sliced-images-consume-dummies
                                               (equal
                                                (buffer-substring-no-properties
                                                 (pos-bol 2)
                                                 (pos-eol 2))
                                                " "))
                                          ;; Consume next line as dummy
                                          (forward-line)
                                        ;; Create dummy line
                                        (end-of-line 1)
                                        (insert (propertize "\n" 'line-height t))
                                        (insert " "))
                                      (setq start (pos-bol)
                                            end (pos-eol))
                                      (if (not dummy-zone-start)
                                          (setq dummy-zone-start start)))
                                    (setq dummy-zone-end end)
                                    (push (org--make-inline-image-overlay
                                           start
                                           end
                                           (list (list 'slice 0 y 1.0 dy) image))
                                          ovfam))
                                  (setq y (+ y dy))))
                              (push (make-overlay dummy-zone-start dummy-zone-end) ovfam)
                              (push ovfam org-inline-image-overlay-families))))))))))))))))

;;;###autoload
(defun org-display-inline-remove-overlay-family (ov after _beg _end &optional _len)
  "Remove inline-display overlay family if a corresponding region is modified."
  (when (and ov after)
    (when (overlay-get ov 'org-image-overlay)
      (image-flush (cadr (overlay-get ov 'display))))
	(catch 'break
      (dolist (ovfam org-inline-image-overlay-families)
        (when (memq ov ovfam)
          (setq org-inline-image-overlay-families
                (delq ovfam org-inline-image-overlay-families))
          (org--delete-inline-image-overlay-family ovfam)
          (throw 'break nil))))))

;;;###autoload
(defun org-remove-inline-images (&optional beg end)
  "Remove inline display of images which start between BEG and END."
  (interactive)
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end)))
    (dolist (ovfam org-inline-image-overlay-families)
      (when (memq (car (last ovfam)) overlays)
        (setq org-inline-image-overlay-families
              (delq ovfam org-inline-image-overlay-families))
        (org--delete-inline-image-overlay-family ovfam)))))

;; ---

(provide 'org-sliced-images)

;;; org-sliced-images.el ends here
