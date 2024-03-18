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

### Dummy lines

Dummy lines are added to support slice overlays. Lines matching dummy lines
coming directly after a link will be overlaid, instead of adding new ones, if
`org-sliced-images-consume-dummies` is t.

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
