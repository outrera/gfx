(defsystem gfx
  :name "gfx"
  :description "Graphics manipulation routines; including png read/save functionality"
  :license "MIT"
  :author "Nikita Sadkov"
  :version "0.1"
  :serial t
  :components
    ((:file "gfx")
     (:file "zlib")
     (:file "png")
     (:file "pcx")
     ))
