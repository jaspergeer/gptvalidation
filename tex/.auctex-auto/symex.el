(TeX-add-style-hook
 "symex"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "graphicx"
    "mathpartir"
    "geometry"
    "syntax"
    "amsmath"
    "amssymb"
    "centernot")
   (TeX-add-symbols
    '("br" 1)
    "Yields"
    "Dom"
    "Ptr"
    "Sel"
    "Upd"
    "Max"
    "ArrTy"
    "PtrTy"
    "NewArr"))
 :latex)

