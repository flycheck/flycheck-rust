flycheck-rust — Flycheck for Rust
=================================

[![Build Status][travis-badge]][travis-url]
[![License GPL 3][badge-license]][copying]
[![MELPA][MELPA-badge]][MELPA-link]

This Flycheck extension configures Flycheck automatically for the current
Cargo project.

Setup
-----

Install from [MELPA][] or [MELPA Stable][].

If you use [`Cask`][cask]:

```emacs-lisp
(source gnu)
(source melpa)

(depends-on "flycheck-rust")
```

Then, in your `init.el`:

```emacs-lisp
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
```

Usage
-----

Just use Flycheck as usual in your Rust/Cargo projects.  `flycheck-rust-setup`
will call `cargo` to determine your project layout and set the variables needed
by the `rust-cargo` checker to properly check your buffer.

You may also want to have a look at [flycheck/flycheck-inline][flycheck-inline],
which works well with diagnostics emitted by Rust.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/flycheck/flycheck-rust/blob/master/COPYING
[Flycheck]: https://github.com/flycheck/flycheck
[Cask]: https://github.com/cask/cask
[MELPA]: http://melpa.milkbox.net
[MELPA Stable]: http://melpa-stable.milkbox.net
[MELPA-badge]: http://melpa.org/packages/flycheck-rust-badge.svg
[MELPA-link]: http://melpa.org/#/flycheck-rust
[flycheck-inline]: https://github.com/flycheck/flycheck-inline
[travis-badge]: https://travis-ci.org/flycheck/flycheck-rust.svg?branch=master
[travis-url]: https://travis-ci.org/flycheck/flycheck-rust
