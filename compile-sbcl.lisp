(require :asdf)
(push "/home/kroger/src/cl-distribution/src/" asdf:*central-registry*)
(require :villa-lobos)
(sb-ext:save-lisp-and-die "villa-lobos" :executable t :toplevel #'villa-lobos:main-binary)

;;(asdf:make-build :villa-lobos :type :program :epilogue-code #'villa-lobos:main-binary)