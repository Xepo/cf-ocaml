open Core.Std

let pi = 3.14159

module T3 = struct
     type 'a t = ('a * 'a * 'a)

     let indices = [`i1; `i2; `i3;]
     let get (c1,c2,c3) = 
          function
          | `i1 -> c1
          | `i2 -> c2
          | `i3 -> c3

     let set : 'a . ('a * 'a * 'a) -> [`i1|`i2|`i3] -> 'a -> ('a *'a*'a) = 
          fun (c1,c2,c3) i v ->
          match i with
          | `i1 -> (v,c2,c3)
          | `i2 -> (c1,v,c3)
          | `i3 -> (c1,c2,v)
end

module Row = struct
     type t = float T3.t

     let cross (i1,i2,i3) (j1,j2,j3) =
          (
               (i2*.j3 -. i3*.j2),
               (i3*.j1 -. i1*.j3),
               (i1*.j2 -. i2*.j1)
          )

     let dot (i1,i2,i3) (j1,j2,j3) = (i1*.j1) +. (i2*.j2) +. (i3*.j3)

end
type t = Row.t T3.t

let create row1 row2 row3 = (row1,row2,row3)

let identity =
     create
          (1.0,0.0,0.0)
          (0.0,1.0,0.0)
          (0.0,0.0,1.0)

let get m i j =
     T3.get (T3.get m i) j

let set : [`i1|`i2|`i3] -> [`i1|`i2|`i3] -> float -> t-> t =
     fun i j v m ->
     T3.set m i (T3.set (T3.get m i) j v) 

let col m j = (get m `i1 j, get m `i2 j, get m `i3 j)
let row = T3.get

let mapi t ~f =
     List.fold T3.indices ~init:t
     ~f:(fun acc i ->
          List.fold T3.indices ~init:acc
          ~f:(fun acc j ->
               let new_val = f (i,j) (get t i j) in
               set i j new_val acc
          ))

module Infix = struct
     let ( *|$ ) m (x,y) : Vec.t = 
          let f (c1,c2,c3) = x *. c1 +. y *. c2 +. c3 in
          (f (row m `i1), f (row m `i2))

     let ( *| ) m1 m2 =
          let row_col i j = 
               List.fold [`i1;`i2;`i3;] ~init:0.0 ~f:(fun acc x ->
                    acc +. (get m1 i x)  *. (get m2 x j))
          in
          create
          ((row_col `i1 `i1),(row_col `i1 `i2), row_col `i1 `i3)
          ((row_col `i2 `i1),(row_col `i2 `i2), row_col `i2 `i3)
          ((row_col `i3 `i1),(row_col `i3 `i2), row_col `i3 `i3)

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
     create
          (col m `i1)
          (col m `i2)
          (col m `i3)

let invert m = 
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
     create
          (cos d, 0. -. sin d, 0.)
          (sin d,    cos d, 0.)
          (0.,0.,1.)
