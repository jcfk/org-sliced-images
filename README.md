# org-sliced-images

This package ports insert-sliced-image functionality into org-mode, with respect
to inline image display. This is supposed to make scrolling over large images
more visually pleasing. It is meant to override a number org's default image
display functions.

## Installation

Place the following aliases in your init.

```
(defalias 'org-remove-inline-images #'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images #'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images #'org-sliced-images-display-inline-images)
```

## Usage

The package is intended to be used exactly like vanilla org, on account of the
above aliases.

Concerning BEG and END arguments to the some of the functions, the beginning of
the link to the image is the point considered.

### Customization

- `org-sliced-images-consume-dummies` Dummy lines are used to support slice
  overlays. If non-nil, lines matching dummy lines coming directly after a link
  will be overlaid, instead of adding new ones.
- `org-sliced-images-round-image-height` If non-nil, round the height of images
  to be a multiple of the font height. This is useful for avoiding gaps when
  prefixing display lines with extra characters, as with `org-indent-mode` or
  line numbers.

## Comparisons

Here's how this package compares to other attempts to make image scrolling nice.

- [iscroll](https://github.com/casouri/iscroll): iscroll works by making you
  rebind your movement keys to a function that calculates and adds a vertical
  scroll. org-sliced-images works by inserting images as slices when they are
  toggled; you can then move over these slices however you want, resulting in a
  faster and more "native" experience.
- pixel-scroll -based methods: These only work with the mouse wheel and are very
  resource intensive.

## Todo

- Remove gap above last slice when using org-indent-mode
