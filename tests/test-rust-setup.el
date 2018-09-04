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
(require 'buttercup)

(buttercup-define-matcher :to-equal-one-of (elt &rest seq)
  (let ((elt (funcall elt))
        (seq (mapcar #'funcall seq)))
    (if (member elt seq)
        (cons t (format "Expected %S not to equal a member of %S" elt seq))
      (cons nil (format "Expected %S to equal a member of %S" elt seq)))))

(defun crate-file (file-name)
  (expand-file-name file-name "tests/test-crate"))

(defun crate-with-features-file (file-name)
  (expand-file-name file-name "tests/crate-with-features"))

(defun lib-crate-file (file-name)
  (expand-file-name file-name "tests/custom-lib-target"))

(defun build-script-crate-file (file-name)
  (expand-file-name file-name "tests/build-script-test"))

(defun cdrassoc (sym alist) (cdr (assoc sym alist)))

(defun get-cargo-version ()
  (let ((cargo (funcall flycheck-executable-find "cargo")))
    (with-output-to-string
      (call-process cargo nil standard-output nil "--version"))))

(defun cargo-version ()
  (pcase-let
      ((`(,ignored1 ,version ,ignored2) (split-string (get-cargo-version))))
    (split-string version "-")))

(defun cargo-older-than-1.29-nightly ()
  (pcase-let ((`(,version ,channel) (cargo-version)))
    (or (version< version "1.29")
        (and (version= version "1.29") (string= channel "beta")))))

(describe
 "`flycheck-rust-find-cargo-target' associates"

 (it "'src/lib.rs' to the library target"
     (expect
      (cdrassoc 'kind (flycheck-rust-find-cargo-target (crate-file "src/lib.rs")))
      :to-equal "lib"))

 (it "'src/a.rs' to the library target"
     (expect
      (cdrassoc 'kind (flycheck-rust-find-cargo-target (crate-file "src/a.rs")))
      :to-equal "lib"))

 (it "'src/main.rs' to the main binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/main.rs"))
      :to-equal '((kind . "bin") (name . "test-crate"))))

 (it "'src/bin/a.rs' to the 'a' binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/a.rs"))
      :to-equal '((kind . "bin") (name . "a"))))

 (it "'src/bin/b.rs' to the 'b' binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/b.rs"))
      :to-equal '((kind . "bin") (name . "b"))))

 (it "'src/bin/support/mod.rs' to any binary target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "src/bin/support/mod.rs"))
      :to-equal-one-of
      '((kind . "bin") (name . "a"))
      '((kind . "bin") (name . "b"))))

 (it "'tests/a.rs' to the 'a' test target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "tests/a.rs"))
      :to-equal '((kind . "test") (name . "a"))))

 (it "'tests/support/mod.rs' to any test target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "tests/support/mod.rs"))
      :to-equal-one-of
      '((kind . "test") (name . "a"))
      '((kind . "test") (name . "b"))))

 (it "'examples/a.rs' to the 'a' example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/a.rs"))
      :to-equal '((kind . "example") (name . "a"))))

 (it "'examples/b.rs' to the 'b' example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/b.rs"))
      :to-equal '((kind . "example") (name . "b"))))

 (it "'examples/support/mod.rs' to any example target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "examples/support/mod.rs"))
      :to-equal-one-of
      '((kind . "example") (name . "a"))
      '((kind . "example") (name . "b"))))

 (it "'benches/a.rs' to the 'a' bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/a.rs"))
      :to-equal '((kind . "bench") (name . "a"))))

 (it "'benches/b.rs' to the 'b' bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/b.rs"))
      :to-equal '((kind . "bench") (name . "b"))))

 (it "'benches/support/mod.rs' to any bench target"
     (expect
      (flycheck-rust-find-cargo-target (crate-file "benches/support/mod.rs"))
      :to-equal-one-of
      '((kind . "bench") (name . "a"))
      '((kind . "bench") (name . "b"))))

 (it "'src/lib.rs' to the library target (custom-lib-target)"
     (expect
      (car (flycheck-rust-find-cargo-target (lib-crate-file "src/lib.rs")))
      :to-equal '(kind . "lib")))

 (it "'build.rs' to any target in the same workspace member (parent)"
     (expect
      (flycheck-rust-find-cargo-target (build-script-crate-file "build.rs"))
      :to-equal '((kind . "bin") (name . "build-script-test"))))

 (it "'build.rs' to any target in the same workspace member (child)"
     (expect
      (flycheck-rust-find-cargo-target (build-script-crate-file "lib-test/build.rs"))
      :to-equal '((kind . "lib") (name . "lib-test"))))

 (it "'src/main.rs' to the bin target with required-features (fea1)"
     (when (cargo-older-than-1.29-nightly)
         (signal 'buttercup-pending "requires cargo 1.29"))
     (expect
      (flycheck-rust-find-cargo-target (crate-with-features-file "src/main.rs"))
      :to-equal '((kind . "bin") (name . "main") (required-features . ("fea1")))))

 (it "'example/example.rs' to the example target with required-features (fea1 fea2)"
     (when (cargo-older-than-1.29-nightly)
         (signal 'buttercup-pending "requires cargo 1.29"))
     (expect
      (flycheck-rust-find-cargo-target (crate-with-features-file "example/example.rs"))
      :to-equal '((kind . "example")
                  (name . "example")
                  (required-features . ("fea1" "fea2")))))
 )
