open Core.Std

type t = float * float

let t2_order x1 x2 = if x1 <. x2 then (x1,x2) else (x2,x1)
let of_ints (x,y) = (float_of_int x, float_of_int y)

let in_square (x1,y1) (x2,y2) (x,y) =
     let (x1,x2) = t2_order x1 x2 in
     let (y1,y2) = t2_order y1 y2 in
     if x >=. x1 && x <=. x2 && y >=. y1 && y <=. y2
     then true
     else false

let origin = (0.0,0.0)

let unitvec = (1.0,1.0)

let in_unit = in_square (-0.5, -0.5) (0.5, 0.5)

module Infix = struct
     let ( =$ ) ((x1,y1):t) ((x2,y2):t) : bool= ((Float.(=) x1 x2) && (Float.(=) y1 y2))
end

let to_string (x,y) = sprintf "(%f,%f)" x y

let assert_equal v1 v2 =
     let open Infix in
     if not (v1 =$ v2)
     then
          printf "ASSERT FAILED: %s != %s" (to_string v1) (to_string v2);
     assert (v1 =$ v2)
