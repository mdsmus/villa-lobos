(require :asdf)
(push "/home/kroger/src/cl-distribution/src/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :villa-lobos)
(save-application "villa-lobos" :toplevel-function #'villa-lobos:main-binary :prepend-kernel "/usr/local/ccl/lx86cl" :error-handler #'quit)
