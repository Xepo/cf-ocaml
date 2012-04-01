type t = Matrix.t

let (=$) = Vec.(=$)
let ( *|$ ) = Matrix.( *|$ )
let ( *| ) = Matrix.( *|)
let ( =| ) = Matrix.( =|)

let iterate_unit ~f output t = 
     let rc = Rect.expand_unit t in
     let it = Matrix.invert t in
     Outputtable.iter rc output ~f:(fun vec ->
          let tvec = it *|$ vec in
          if Vec.in_unit tvec
          then 
               f tvec
          else
               None
     )

let size t =
     let (cxx,cxy,_) = (Matrix.row t `i1) in
     let (cyx,cyy,_) = (Matrix.row t `i2) in
     min (cxx +. cxy) (cyx +. cyy)
