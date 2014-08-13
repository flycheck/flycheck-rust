;;; flycheck-rust.el --- Flycheck: Rust additions and Cargo support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-rust
;; Keywords: tools, convenience
;; Version: 0.1-cvs
;; Package-Requires: ((flycheck "0.20") (dash "2.4.0"))

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

;; Improve Rust support in Flycheck by configuring Flycheck automatically in
;; Cargo projects.

;;;; Setup

;; (add-hook 'rust-mode-hook #'flycheck-rust-setup)

;;; Code:

;;;###autoload
(defun flycheck-rust-setup ()
  "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout.")

(provide 'flycheck-rust)

;;; flycheck-rust.el ends here
