(in-package :asdf)

(defsystem :icfp
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:alexandria
               :cl-json
	       :ltk))

