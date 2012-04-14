open Core.Std

let pi = 3.14159

module T3 = struct
     type 'a t = ('a * 'a * 'a)

     type index =  [`i1|`i2|`i3]
     let indices = [`i1;`i2;`i3;]
     let get i (c1,c2,c3) =
          match i with
          | `i1 -> c1
          | `i2 -> c2
          | `i3 -> c3

     let set i v (c1,c2,c3) =
          match i with
          | `i1 -> (v,c2,c3)
          | `i2 -> (c1,v,c3)
          | `i3 -> (c1,c2,v)
     let create v = (v,v,v)
end

module Row = struct
     type t = float T3.t
     let indices = T3.indices
     let create = T3.create
     let get = T3.get
     let set = T3.set

     let cross (i1,i2,i3) (j1,j2,j3) =
          (
               (i2*.j3 -. i3*.j2),
               (i3*.j1 -. i1*.j3),
               (i1*.j2 -. i2*.j1)
          )

     let dot (i1,i2,i3) (j1,j2,j3) = (i1*.j1) +. (i2*.j2) +. (i3*.j3)
end
module Mat = struct
     type t = Row.t T3.t
     type index = T3.index
     let indices = T3.indices
     let create = T3.create
     let get = T3.get
     let set = T3.set
end
type t = Mat.t

let get i j m =
     Row.get j (Mat.get i m)

let set i j v t = 
     let row = Row.set j v (Mat.get i t) in
     Mat.set i row t

let create v = Mat.create (Row.create v)
let indices = 
     List.fold Row.indices ~init:[] ~f:(fun acc i -> 
          List.fold Row.indices ~init:acc ~f:(fun acc j ->
               (i,j) :: acc))

let initialize (f:(Mat.index * Mat.index) -> float) = 
     let init = Mat.create (Row.create 0.0) in
     List.fold indices ~init
          ~f:(fun acc (i,j) -> set i j (f (i,j)) acc)

let identity =
     (
          (1.0,0.0,0.0),
          (0.0,1.0,0.0),
          (0.0,0.0,1.0)
     )

let col j m = (get `i1 j m, get `i2 j m, get `i3 j m)
let row = Mat.get

let mapi t ~f =
     initialize (fun (i,j) ->
          f (i,j) (get i j t) )

let map ~f = mapi ~f:(fun (_,_) v -> f v)

let multiply t1 t2 =
     let do_cell (i,j) = Row.dot (row i t1) (col j t2) in
     initialize do_cell

let mul_vec t vec = 
     (
          Row.dot (row `i1 t) vec,
          Row.dot (row `i2 t) vec,
          Row.dot (row `i3 t) vec
     )

module Infix = struct
     let ( *|$ ) m (x,y) : Vec.t =
          let f (c1,c2,c3) = x *. c1 +. y *. c2 +. c3 in
          (f (row `i1 m), f (row `i2 m))

     let ( *| ) = multiply
     (*
     let ( *| ) m1 m2 =
          let row_col i j =
               List.fold [`i1;`i2;`i3;] ~init:0.0 ~f:(fun acc x ->
                    acc +. (get i x m1) *. (get x j m2))
          in
          (
          ((row_col `i1 `i1),(row_col `i1 `i2), row_col `i1 `i3),
          ((row_col `i2 `i1),(row_col `i2 `i2), row_col `i2 `i3),
          ((row_col `i3 `i1),(row_col `i3 `i2), row_col `i3 `i3)
          )
          *)

     let ( =| ) m1 m2 = m1 = m2

     let ( *|. ) s m1 = mapi m1 ~f:(fun _ old -> old *. s)
end
include Infix



let det m =
     let (row1,row2,row3) = m in
     let det = Row.dot row1 (Row.cross row2 row3) in
     assert (not (Float.(=) det 0.));
     det

let transpose m =
     (
          (col `i1 m),
          (col `i2 m),
          (col `i3 m)
     )

let invert m =
     let idet = 1. /. (det m) in
     let col1 = col `i1 m in
     let col2 = col `i2 m in
     let col3 = col `i3 m in
     let newm =
          (
               (Row.cross col2 col3),
               (Row.cross col3 col1),
               (Row.cross col1 col2)
          )
     in
     idet *|. newm


let to_string (r1,r2,r3) =
     let row_to_string (c1,c2,c3) = sprintf "%f,%f,%f" c1 c2 c3 in
     sprintf "[%s\n %s\n %s]"
     (row_to_string r1)
     (row_to_string r2)
     (row_to_string r3)

let translate x y =
     identity
     |! set `i1 `i3 x
     |! set `i2 `i3 y

let scale x y =
     identity
     |! set `i1 `i1 x
     |! set `i2 `i2 y

let rotate d =
     let d = (d *. pi /. 180.) in
     (
          (cos d, 0. -. sin d, 0.),
          (sin d, cos d, 0.),
          (0.,0.,1.)
     )





          (*
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
*)
