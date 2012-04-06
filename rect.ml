open Core.Std

type t = Vec.t * Vec.t
let ( *|$ ) = Matrix.( *|$ )

let normalize ((x1,y1),(x2,y2)) =
     (((min x1 x2), (min y1 y2)), ((max x1 x2), (max y1 y2)))

let expand r (v1,v2) =
     let r = normalize r in
     let f  (x3,y3) ((x1,y1),(x2,y2)) =
          (min x1 x3, min y1 y3), (max x2 x3, max y2 y3)
     in
     f v2 (f v1 r)

let apply (v1,v2) m = 
     (m *|$ v1, m *|$ v2)

let expand_and_apply t r1 r2 =
     expand (apply r1 t) (apply r2 t)

let to_string (v1,v2) = 
     sprintf "(%s-%s)" (Vec.to_string v1) (Vec.to_string v2)

let unit_rect = ((-0.5,-0.5), (0.5,0.5))

let normalize ((x1,y1),(x2,y2)) =
     (((min x1 x2), (min y1 y2)), ((max x1 x2), (max y1 y2)))

let expand_unit t =
     expand_and_apply t unit_rect ((-.0.5, 0.5),(0.5,-.0.5))

let pt_in_rect r (x,y) = 
     let ((x1,y1),(x2,y2)) = normalize r in
     ((x1 <= x) && (x <= x2)) && ((y1 <= y) && (y <= y2))

let width ((x1,_),(x2,_)) = x2 -. x1
let height ((_,y1),(_,y2)) = y2 -. y1
