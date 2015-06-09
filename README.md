flycheck-rust — Flycheck for Rust
=================================

[![License GPL 3][badge-license]][copying]

Configure [Flycheck][] for Rust.

- Setup Flycheck to follow the Cargo project layout.

Installation
------------

As usual, from [MELPA][] or [MELPA Stable][].

In your [`Cask`][cask] file:

```cl
(source gnu)
(source melpa)

(depends-on "flycheck-rust")
```

In your `init.el`:

```cl
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
```

Usage
-----

Just use Flycheck as usual in your Rust/Cargo projects.

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
