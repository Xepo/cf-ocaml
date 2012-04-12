open Core.Std

let pi = 3.14159

include Generic_matrix.Matrix3

module Infix = struct
     include Infix_g
     let ( *|$ ) m (x,y) : Vec.t = 
          let (x,y,_) = Generic_matrix.Matrix3.mul_vec m (x,y,1.) in
          x,y

     let ( *| ) m1 m2 = Generic_matrix.Matrix3.multiply m1 m2
     let ( =| ) m1 m2 = m1 = m2

     let ( *|. ) s m1 = mapi m1 ~f:(fun _ old -> old *. s)
end

module Row = struct
     include Row_g
     let cross (i1,i2,i3) (j1,j2,j3) =
          (
               (i2*.j3 -. i3*.j2),
               (i3*.j1 -. i1*.j3),
               (i1*.j2 -. i2*.j1)
          )

end


let create r1 r2 r3 = (r1,r2,r3)
let det m = 
     let (row1,row2,row3) = m in
     let det = Row.dot row1 (Row.cross row2 row3) in
     assert (not (Float.(=) det 0.));
     det

let invert m = 
     let open Infix in
     let idet = 1. /. (det m) in
     let col1 = col m `i1 in
     let col2 = col m `i2 in
     let col3 = col m `i3 in
     let newm = 
          create
               (Row.cross col2 col3)
               (Row.cross col3 col1)
               (Row.cross col1 col2)
     in
     idet *|. newm

let scale : float -> float -> t = fun x y  -> scale (x,y,1.)
let translate x y : t = translate (x,y,1.)
let rotate d = rotate `i1 `i2 d
