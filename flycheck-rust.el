;;; flycheck-rust.el --- Flycheck: Rust additions and Cargo support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-rust
;; Keywords: tools, convenience
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "24.1") (flycheck "0.20") (dash "2.4.0"))

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

;; This Flycheck extension configures Flycheck automatically for the current
;; Cargo project.
;;
;; # Setup
;;
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;
;; # Usage
;;
;; Just use Flycheck as usual in your Rust/Cargo projects.
;;
;; Note: You must run `cargo build` initially to install all dependencies.  If
;; you add new dependencies to `Cargo.toml` you need to run `cargo build`
;; again. Otherwise you will see spurious errors about missing crates.

;;; Code:

(require 'dash)
(require 'flycheck)

(defun flycheck-rust-executable-p (rel-name)
  "Whether REL-NAME denotes an executable.

REL-NAME is the file relative to the Cargo.toml file."
  (or (string= "src/main.rs" rel-name)
      (string-prefix-p "src/bin/" rel-name)))

(defun flycheck-rust-test-p (rel-name)
  "Whether REL-NAME denotes a test.

REL-NAME is the file relative to the Cargo.toml file."
  (string-prefix-p "tests/" rel-name))

(defun flycheck-rust-bench-p (rel-name)
  "Whether REL-NAME denotes a bench.

REL-NAME is the file relative to the Cargo.toml file."
  (string-prefix-p "benches/" rel-name))

(defun flycheck-rust-example-p (rel-name)
  "Whether REL-NAME denotes an example.

REL-NAME is the file relative to the Cargo.toml file."
  (string-prefix-p "examples/" rel-name))

(defun flycheck-rust-project-root ()
  "Get the project root for the current buffer.

Return the directory containing the Cargo file, or nil if there
is none."
  (locate-dominating-file (buffer-file-name) "Cargo.toml"))

(defun flycheck-rust-find-crate-root ()
  "Get the crate root (the nearest lib.rs or main.rs)
relative to the current file."
  (-if-let (lib-crate-dir (locate-dominating-file (buffer-file-name) "lib.rs"))
      (expand-file-name "lib.rs" lib-crate-dir)
    (-when-let (exe-crate-dir (locate-dominating-file (buffer-file-name) "main.rs"))
      (expand-file-name "main.rs" exe-crate-dir))))

;;;###autoload
(defun flycheck-rust-setup ()
  "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout."
  (interactive)
  (when (buffer-file-name)
    (-when-let (root (flycheck-rust-project-root))
      (let ((rel-name (file-relative-name (buffer-file-name) root)))
        ;; These are valid crate roots as by Cargo's layout
        (unless (or (flycheck-rust-executable-p rel-name)
                    (flycheck-rust-test-p rel-name)
                    (flycheck-rust-bench-p rel-name)
                    (flycheck-rust-example-p rel-name)
                    (string= "src/lib.rs" rel-name))
          ;; For other files, the library is either the default library or the
          ;; executable
          (setq-local flycheck-rust-crate-root (flycheck-rust-find-crate-root)))
        ;; Check tests in libraries and integration tests
        (setq-local flycheck-rust-check-tests
                    (not (flycheck-rust-executable-p rel-name)))
        ;; Set the crate type
        (setq-local flycheck-rust-crate-type
                    (if (flycheck-rust-executable-p rel-name) "bin" "lib"))
        ;; Find build libraries
        (setq-local flycheck-rust-library-path
                    (list (expand-file-name "target/debug" root)
                          (expand-file-name "target/debug/deps" root)))))))

(provide 'flycheck-rust)

;;; flycheck-rust.el ends here
