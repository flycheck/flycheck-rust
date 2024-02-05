(package "flycheck-rust"
         "1.1"
         "Flycheck: Rust additions and Cargo support")

(website-url "https://github.com/flycheck/flycheck-rust")
(keywords "tools" "convenience")

(package-file "flycheck-rust.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "24.1")
(depends-on "flycheck" "28")
(depends-on "dash" "2.13.0")
(depends-on "seq" "2.3")
(depends-on "let-alist" "1.0.4")

(development
 (depends-on "buttercup")  ; BDD test framework for Emacs
 (depends-on "rust-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
