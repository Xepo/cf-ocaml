open Core.Std

let pi = 3.14159
module Row = struct
     type t = float * float * float
end

type t = Row.t * Row.t * Row.t

let create row1 row2 row3 = (row1,row2,row3)

let identity =
     create
          (1.0,0.0,0.0)
          (0.0,1.0,0.0)
          (0.0,0.0,1.0)

let t3_get (c1,c2,c3) = 
     function
     | `i1 -> c1
     | `i2 -> c2
     | `i3 -> c3

let t3_set : 'a . ('a * 'a * 'a) -> [`i1|`i2|`i3] -> 'a -> ('a *'a*'a) = 
     fun (c1,c2,c3) i v ->
     match i with
     | `i1 -> (v,c2,c3)
     | `i2 -> (c1,v,c3)
     | `i3 -> (c1,c2,v)

let int_to_i = 
     function
     | 1 -> `i1
     | 2 -> `i2
     | 3 -> `i3
     | _ as i -> failwithf "Invalid loc: %d\n" i ()

let get_i m i j =
     t3_get (t3_get m i) j

let set_i : t -> [`i1|`i2|`i3] -> [`i1|`i2|`i3] -> float -> t =
     fun m i j v ->
     t3_set m i (t3_set (t3_get m i) j v) 

let l_to_i = 
     function
     | `l11 -> `i1,`i1
     | `l12 -> `i1,`i2
     | `l13 -> `i1,`i3
     | `l21 -> `i2,`i1
     | `l22 -> `i2,`i2
     | `l23 -> `i2,`i3
     | `l31 -> `i3,`i1
     | `l32 -> `i3,`i2
     | `l33 -> `i3,`i3

let get ~l m =
     let (i,j) = l_to_i l in
     get_i m i j

let set ~l ~v m =
     let (i,j) = l_to_i l in
     set_i m i j v

let col m j = (get_i m `i1 j, get_i m `i2 j, get_i m `i3 j)
let row = t3_get

let ( *|$ ) m (x,y) : Vec.t = 
     let f (c1,c2,c3) = x *. c1 +. y *. c2 +. c3 in
     (f (row m `i1), f (row m `i2))

let ( *| ) m1 m2 =
     let row_col i j = 
          List.fold [`i1;`i2;`i3;] ~init:0.0 ~f:(fun acc x ->
               acc +. (get_i m1 i x)  *. (get_i m2 x j))
     in
     create
     ((row_col `i1 `i1),(row_col `i1 `i2), row_col `i1 `i3)
     ((row_col `i2 `i1),(row_col `i2 `i2), row_col `i2 `i3)
     ((row_col `i3 `i1),(row_col `i3 `i2), row_col `i3 `i3)

let map m ~f =
     let z l m = set m ~l ~v:(f (l_to_i l) (get m ~l)) in
     z `l11 m
     |! z `l12
     |! z `l13
     |! z `l21
     |! z `l22
     |! z `l23
     |! z `l31
     |! z `l32
     |! z `l33


let cross (i1,i2,i3) (j1,j2,j3) =
     (
          (i2*.j3 -. i3*.j2),
          (i3*.j1 -. i1*.j3),
          (i1*.j2 -. i2*.j1)
     )

let dot (i1,i2,i3) (j1,j2,j3) = (i1*.j1) +. (i2*.j2) +. (i3*.j3)

let ( =| ) m1 m2 = m1 = m2
let ( *|. ) s m1 = map m1 ~f:(fun _ old -> old *. s)


let det m = 
     let (row1,row2,row3) = m in
     let det = dot row1 (cross row2 row3) in
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
               (cross col2 col3)
               (cross col3 col1)
               (cross col1 col2)
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
     |! set ~l:`l13 ~v:x
     |! set ~l:`l23 ~v:y

let scale x y =
     identity
     |! set ~l:`l11 ~v:x
     |! set ~l:`l22 ~v:y

let rotate d = 
     let d = (d *. pi /. 180.) in
     create
          (cos d, 0. -. sin d, 0.)
          (sin d,    cos d, 0.)
          (0.,0.,1.)
