;;;; calcu-core.asd

(asdf:defsystem #:calcu-core
  :description "Abstract emulation of a basic calculator"
  :author "Stanislav Kondratyev"
  :license "WTFPL"
  :serial t
  :components ((:file "package")
               (:file "calcu-core")))

