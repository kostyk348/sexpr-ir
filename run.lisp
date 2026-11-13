;;; run.lisp — прогон демо и корпуса. Точка входа для скрипта.
;;;
;;;   SBCL_HOME=/usr/local/sbcl/lib sbcl --script run.lisp
;;;
;;; Код выхода: 0 — все случаи прошли, 1 — есть провалы.

(load (merge-pathnames "ir.lisp" *load-pathname*))

(sb-ext:exit :code (if (demo) 0 1))
