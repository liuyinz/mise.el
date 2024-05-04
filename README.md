# mise

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/mise-badge.svg)](http://melpa.org/#mise)

A GNU Emacs library which uses the [mise][mise] tool
to determine per-directory/project environment variables and then set
those environment variables on a per-buffer basis.

Heavily inspired from [envrc][envrc] which created by Purcell.

<!-- markdown-toc start -->

## Contents

- [mise](#mise)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [Todo](#todo)
  - [Donate](#donate)

<!-- markdown-toc end -->

## Install

### dependencies

- [inheirtenv](https://github.com/purcell/inheritenv)
- [mise][mise], version >= 2024.4.8
- emacs, version >= 29.1

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA]. Install with `M-x package-install` `RET` `mise` within Emacs.

## Usage

```elisp
;; Directly
(require mise)

;; enable globally
(add-hook 'after-init-hook #'global-mise-mode)

;; or turn on in some buffer
(add-hook 'emacs-lisp-mode-hook #'mise-mode)
```

## Customization

- `mise-update-on-eshell-directory-change`: enable it would update environment when changing directory in eshell.

- `mise-exclude-predicate`: conditional function to exclude buffers which shouldn't turn on `mise-mode`.

- `mise-auto-propagate-commands`: list of commands which running with mise environment activated.

## Todo

- [ ] support more options in `mise` cli

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<a href="https://paypal.me/liuyinz" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>

[melpa]: http://melpa.org/#/git-cliff
[mise]: https://mise.jdx.dev/
[envrc]: https://github.com/purcell/envrc
