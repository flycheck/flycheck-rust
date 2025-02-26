(package "flycheck-rust"
         "1.1"
         "Flycheck: Rust additions and Cargo support")

(website-url "https://github.com/flycheck/flycheck-rust")
(keywords "tools" "convenience")

(package-file "flycheck-rust.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "flycheck")
(depends-on "dash")
(depends-on "seq")
(depends-on "let-alist")

(development
 (depends-on "buttercup")  ; BDD test framework for Emacs
 (depends-on "rust-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
