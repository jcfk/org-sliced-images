# org-sliced-images

```
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
```
