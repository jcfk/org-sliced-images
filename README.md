# org-sliced-images [![MELPA](https://melpa.org/packages/org-sliced-images-badge.svg)](https://melpa.org/#/org-sliced-images)

This package displays org-mode inline images in a sliced manner, like the
built-in `insert-sliced-image`. This improves the image scrolling experience.

![](media/example.gif)

## Installation

Get me on MELPA! `use-package` example:

```elisp
(use-package org-sliced-images
  :ensure t
  :config
  (org-sliced-images-mode 1))
```

## Usage

Toggle the global minor mode `org-sliced-images-mode` to enable sliced images.
The mode advises the following functions:

- `org-remove-inline-images`
- `org-toggle-inline-images`
- `org-display-inline-images`

Concerning BEG and END arguments to the some of the functions, the beginning of
the link to the image is the point considered.

### Customization

- `org-sliced-images-consume-dummies` Dummy lines are used to support slice
  overlays. If non-nil, lines matching dummy lines coming directly after a link
  will be overlaid, instead of adding new ones.
- `org-sliced-images-round-image-height` If non-nil, round the height of images
  to be a multiple of the font height. This should be used with things like
  `org-indent-mode` or line numbering that add prefixes to slice lines, or else
  you will typically have a visible gap above the final slice.

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

