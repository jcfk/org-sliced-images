;;; org-sliced-images.el --- Sliced inline images in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/jcfk/org-sliced-images

;; Package-Requires: ((emacs "29.1") (org "9.6.15"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Display images as sliced images in org-mode, like insert-sliced-image. This
;; makes scrolling nicer.

;; See https://github.com/jcfk/org-sliced-images for more information.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-attach)

;; Configuration

(defgroup org-sliced-images nil
  "Configure org-sliced-images."
  :group 'org)

(defcustom org-sliced-images-consume-dummies t
  "If non-nil, overlay existing dummy lines instead of adding new ones."
  :type 'boolean
  :group 'org-sliced-images)

(defcustom org-sliced-images-round-image-height nil
  "If non-nil, resize images to make height a multiple of the font height.

This is useful for avoiding gaps when prefixing display lines with extra
characters. The image height is rounded to the nearest multiple of the
`default-font-height'."
  :type 'boolean
  :group 'org-sliced-images)

;; Buffer variables

(defvar-local org-sliced-images-inline-image-overlay-families nil
  "A list of elements corresponding to displayed inline images.
Each element is a list of overlays making up the displayed image.

The first element in each list is an overlay over all the dummy lines
inserted to support the slices. The remaining elements are the slices
themselves; the last element is the topmost slice.")
(put 'org-sliced-images-inline-image-overlay-families 'permanent-local t)

;; Function overrides

(defun org-sliced-images--remove-overlay-family (ovfam)
  "Delete the overlay family OVFAM."
  (dolist (ov (cdr ovfam))
    (delete-overlay ov))
  (let ((inhibit-modification-hooks nil))
    (delete-region (overlay-start (car ovfam)) (overlay-end (car ovfam))))
  (delete-overlay (car ovfam)))

(defun org-sliced-images--inline-image-overlay-families (&optional beg end)
  "Return image overlay families which start between BEG and END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end))
         result)
    (dolist (ovfam org-sliced-images-inline-image-overlay-families result)
      (when (memq (car (last ovfam)) overlays)
        (push ovfam result)))))

;;;###autoload
(defun org-sliced-images-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images starting between BEG and END.
INCLUDE-LINKED is passed to `org-sliced-images-display-inline-images'."
  (interactive "P")
  (if (org-sliced-images--inline-image-overlay-families beg end)
      (progn
        (org-sliced-images-remove-inline-images beg end)
        (when (called-interactively-p 'interactive)
          (message "Inline image display turned off")))
    (org-sliced-images-display-inline-images include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org-sliced-images--inline-image-overlay-families beg end)))
        (message (if new
                     (format "%d images displayed inline"
                             (length new))
                   "No images to display inline"))))))

(defun org-sliced-images--create-inline-image (file width)
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

(defun org-sliced-images--make-inline-image-overlay (start end spec)
  "Make overlay from START to END with display value SPEC.
The overlay is returned."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'display spec)
    (overlay-put ov 'face 'default)
    (overlay-put ov 'org-image-overlay t)
    (overlay-put ov 'modification-hooks
                 (list 'org-sliced-images--handle-modified-overlay-family))
    (when (boundp 'image-map)
      (overlay-put ov 'keymap image-map))
    ov))

;;;###autoload
(defun org-sliced-images-display-inline-images (&optional include-linked refresh beg end)
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
      (org-sliced-images-remove-inline-images beg end)
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
                        (let ((image (org-sliced-images--create-inline-image file width)))
                          (when image
                            (let* ((image-pixel-cons (image-size image t))
                                   (image-pixel-h (cdr image-pixel-cons))
                                   (font-height (default-font-height)))
                              ;; Round image height
                              (when org-sliced-images-round-image-height
                                (setq image-pixel-h
                                      (truncate (* (fround (/ image-pixel-h font-height 1.0)) font-height)))
                                (setf (image-property image :height) image-pixel-h)
                                (setf (image-property image :width) nil))
                              (let* ((image-line-h (/ image-pixel-h font-height 1.0001))
                                     (y 0.0) (dy (/ image-line-h))
                                     (left-margin nil)
                                     (dummy-zone-start nil)
                                     (dummy-zone-end nil)
                                     (ovfam nil))
                                (image-flush image)
                                (org-with-point-at (org-element-property :begin link)
                                  (when (> (current-column) 0)
                                    (setq left-margin (current-column)))
                                  (while (< y 1.0)
                                    (let (slice-start slice-end)
                                      (if (= y 0.0)
                                          ;; Overlay link
                                          (progn
                                            (setq slice-start (org-element-property :begin link)
                                                  slice-end (org-element-property :end link))
                                            (end-of-line)
                                            (delete-char 1)
                                            (insert (propertize "\n" 'line-height t)))
                                        (setq slice-start (pos-bol)
                                              slice-end (1+ (pos-bol)))
                                        (if (and org-sliced-images-consume-dummies
                                                 (equal (buffer-substring-no-properties
                                                         (pos-bol) (pos-eol))
                                                        " "))
                                            ;; Consume next line as dummy
                                            (progn
                                              (when left-margin
                                                (put-text-property slice-start slice-end
                                                                   'line-prefix `(space :width ,left-margin)))
                                              (put-text-property (pos-eol) (1+ (pos-eol)) 'line-height t)
                                              (forward-line))
                                          ;; Create dummy line
                                          (insert (if left-margin
                                                      (propertize " " 'line-prefix `(space :width ,left-margin))
                                                    " "))
                                          (insert (propertize "\n" 'line-height t)))
                                        (if (not dummy-zone-start)
                                            (setq dummy-zone-start slice-start))
                                        (setq dummy-zone-end (1+ slice-end)))
                                      (push (org-sliced-images--make-inline-image-overlay
                                             slice-start
                                             slice-end
                                             (list (list 'slice 0 y 1.0 dy) image))
                                            ovfam))
                                    (setq y (+ y dy))))
                                (setq end (+ end (* 2 (- (ceiling image-line-h) 1))))
                                (push (make-overlay dummy-zone-start dummy-zone-end) ovfam)
                                (push ovfam org-sliced-images-inline-image-overlay-families)))))))))))))))))

(defun org-sliced-images--handle-modified-overlay-family (ov after _beg _end &optional _len)
  "Remove inline display overlay family if the area is modified.
This function is to be used as an overlay modification hook; OV, AFTER,
BEG, END, LEN will be passed by the overlay."
  (when (and ov after)
    (when (overlay-get ov 'org-image-overlay)
      (image-flush (cadr (overlay-get ov 'display))))
    (catch 'break
      (dolist (ovfam org-sliced-images-inline-image-overlay-families)
        (when (memq ov ovfam)
          (setq org-sliced-images-inline-image-overlay-families
                (delq ovfam org-sliced-images-inline-image-overlay-families))
          (org-sliced-images--remove-overlay-family ovfam)
          (throw 'break nil))))))

;;;###autoload
(defun org-sliced-images-remove-inline-images (&optional beg end)
  "Remove inline display of images starting between BEG and END."
  (interactive)
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end)))
    (dolist (ovfam org-sliced-images-inline-image-overlay-families)
      (when (memq (car (last ovfam)) overlays)
        (setq org-sliced-images-inline-image-overlay-families
              (delq ovfam org-sliced-images-inline-image-overlay-families))
        (org-sliced-images--remove-overlay-family ovfam)))))

(defun org-sliced-images-+org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (org-sliced-images-remove-inline-images beg end)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))
;; Minor mode

;;;###autoload
(define-minor-mode org-sliced-images-mode
  "Toggle org-sliced-images-mode to display sliced images in `org-mode'."
  :group 'org-sliced-images
  :global t
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (derived-mode-p 'org-mode)
          (if org-sliced-images-mode
              (org-remove-inline-images)
            (org-sliced-images-remove-inline-images)))))
  (if org-sliced-images-mode
      (progn
        (advice-add 'org-remove-inline-images :override #'org-sliced-images-remove-inline-images)
        (advice-add 'org-toggle-inline-images :override #'org-sliced-images-toggle-inline-images)
        (add-hook 'before-save-hook 
                  (lambda ()
                    (when (eq major-mode 'org-mode)
                      (org-remove-inline-images))))
        (add-hook 'after-save-hook 
                  (lambda ()
                    (when (eq major-mode 'org-mode)
                      (org-display-inline-images))))
        (advice-add '+org--toggle-inline-images-in-subtree :override #'org-sliced-images-+org--toggle-inline-images-in-subtree )
        (advice-add 'org-display-inline-images :override #'org-sliced-images-display-inline-images))
    (progn
      (advice-remove 'org-remove-inline-images #'org-sliced-images-remove-inline-images)
      (advice-remove 'org-toggle-inline-images #'org-sliced-images-toggle-inline-images)
      (remove-hook 'before-save-hook
                   (lambda ()
                     (when (eq major-mode 'org-mode)
                       (org-remove-inline-images))))
      (remove-hook 'after-save-hook 
                   (lambda ()
                     (when (eq major-mode 'org-mode)
                       (org-display-inline-images))))
      (advice-remove '+org--toggle-inline-images-in-subtree #'org-sliced-images-+org--toggle-inline-images-in-subtree )
      (advice-remove 'org-display-inline-images #'org-sliced-images-display-inline-images))))


(provide 'org-sliced-images)

;;; org-sliced-images.el ends here
