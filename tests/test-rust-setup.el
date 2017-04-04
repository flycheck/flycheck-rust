;;; test-rust-setup.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017 fmdkdd

;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration tests for `flycheck-rust-setup'.

;;; Code:

(require 'flycheck-rust)

(buttercup-define-matcher :to-equal-one-of (elt &rest seq)
  (if (member elt seq)
      (cons t (format "Expected %S not to equal a member of %S" elt seq))
    (cons nil (format "Expected %S to equal a member of %S" elt seq))))

(defun crate-file (file-name)
  (expand-file-name file-name "tests/test-crate"))

(defun lib-crate-file (file-name)
  (expand-file-name file-name "tests/custom-lib-target"))

(describe
 "`flycheck-rust-find-cargo-target' associates"

 (it "'src/lib.rs' to the library target"
     (expect
      (car (flycheck-rust-find-cargo-target (crate-file "src/lib.rs")))
      :to-equal "lib"))

 (it "'src/a.rs' to the library target"
     (expect
      (car (flycheck-rust-find-cargo-target (crate-file "src/a.rs")))
      :to-equal "lib"))

 (it "'src/main.rs' to the main binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/main.rs"))
      :to-equal '("bin" . "test-crate")))

 (it "'src/bin/a.rs' to the 'a' binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/a.rs"))
      :to-equal '("bin" . "a")))

 (it "'src/bin/b.rs' to the 'b' binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/b.rs"))
      :to-equal '("bin" . "b")))

 (it "'src/bin/support/mod.rs' to any binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/support/mod.rs"))
      :to-equal-one-of '("bin". "a") '("bin". "b")))

 (it "'tests/a.rs' to the 'a' test target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "tests/a.rs"))
      :to-equal '("test" . "a")))

 (it "'tests/support/mod.rs' to any test target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "tests/support/mod.rs"))
      :to-equal-one-of '("test". "a") '("test". "b")))

 (it "'examples/a.rs' to the 'a' example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/a.rs"))
      :to-equal '("example" . "a")))

 (it "'examples/b.rs' to the 'b' example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/b.rs"))
      :to-equal '("example" . "b")))

 (it "'examples/support/mod.rs' to any example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/support/mod.rs"))
      :to-equal-one-of '("example" . "a") '("example" . "b")))

 (it "'benches/a.rs' to the 'a' bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/a.rs"))
      :to-equal '("bench" . "a")))

 (it "'benches/b.rs' to the 'b' bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/b.rs"))
      :to-equal '("bench" . "b")))

 (it "'benches/support/mod.rs' to any bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/support/mod.rs"))
      :to-equal-one-of '("bench" . "a") '("bench" . "b")))

 (it "'src/lib.rs' to the library target"
     (expect
      (car (flycheck-rust-find-cargo-target (lib-crate-file "src/lib.rs")))
      :to-equal "lib"))
 )
