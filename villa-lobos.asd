(in-package :asdf)

(defsystem :villa-lobos
    :name "A tool for music research"
    :author "Pedro Kroger"
    :version "0.0"
    :serial t
    :depends-on (:cl-extensions :ltk :swank :aristoxenus)
    :components ((:file "packages")
                 (:file "utils")
                 (:file "gui-basic")
                 (:file "gui")
                 (:file "main")))

(defsystem :villa-lobos-tests
  :depends-on (:villa-lobos :unittest)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :villa-lobos))))
  (operate 'load-op :villa-lobos-tests)
  (funcall (intern (symbol-name :run!) (find-package :villa-lobos-tests))))
