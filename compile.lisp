(require :asdf)
(push "/home/kroger/src/cl-distribution/src/" asdf:*central-registry*)
(require :villa-lobos)
(sb-ext:save-lisp-and-die "villa-lobos" :executable t :toplevel #'villa-lobos:main)
