fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%0m-%0d"))
(filename (file-name-base buffer-file-name))
(filename-title (upcase-initials (s-replace-regexp (rx (or "-" "_")) " " (file-name-base buffer-file-name))))

org-mode

(inline "src_" p "[:exports code]{" r> "}")

rust-ts-mode

(pt "fn pt<T>(_: &T) { println!(\"{}\", std::any::type_name::<T>()) }" q)

typst-ts-mode

(font "#set text(font: \"LXGW WenKai\", size: 13pt) " q)

markdown-mode

(meta "+++
title = \"" (p (upcase-initials (s-replace-regexp (rx (or "-" "_")) " " (file-name-base buffer-file-name)))) """\"
description = \"" p "\"
date = " (format-time-string "%Y-%0m-%0d") "
updated = " (format-time-string "%Y-%0m-%0d") "
draft = false

[taxonomies]
tags = ["(p "\"Emacs\"" )"]

[extra]
math = false
math_auto_render = false
keywords = \"" (p "A, B") "\"
toc = true
series = \"Features\" # TODO
+++
" q)

(more "<!-- more -->" q)

java-ts-mode
(pl "System.out.println(String.format(\"%s\", " q "));")

