# org-sliced-images

```
;; This package ports insert-sliced-image functionality into org-mode, with
;; respect to inline image display. This is supposed to make scrolling over
;; large images more visually pleasing.
;;
;; Usage
;; -----
;;
;; It overrides a number of image display functions, so usage should be very
;; similar to that of vanilla org.
;;
;; Regarding dummy lines
;; ---------------------
;;
;; Dummy lines are added to support slice overlays. Lines matching dummy lines
;; coming directly after a link can be overlaid, instead of adding new ones,
;; depending on the value of `org-sliced-images-consume-dummies'. This prevents
;; a massive buildup of dummy lines over multiple file saves.
```
