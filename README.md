# org-sliced-images

This package ports insert-sliced-image functionality into org-mode, with respect
to inline image display. This is supposed to make scrolling over large images
more visually pleasing.

## Installation

Be sure to re-bind your toggle key sequence to
`org-sliced-images-toggle-inline-images`.

## Usage

These are analogous to their `org-` counterparts:

- `org-sliced-images-toggle-inline-images` toggles the display of images
- `org-sliced-images-display-inline-images` displays images
- `org-sliced-images-remove-inline-images` removes images

Concerning BEG and END arguments to the above functions, the beginning of the
link to the image is the point considered.

### Dummy lines

Dummy lines are added to support slice overlays. Lines matching dummy lines
coming directly after a link will be overlaid, instead of adding new ones, if
`org-sliced-images-consume-dummies` is t.


