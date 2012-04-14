open Core.Std


let assert_eq eq to_str x y = 
     if eq x y 
     then true
     else
          (
          printf "Assert failure: %s vs. %s\n" (to_str x) (to_str y);
          false
          )
let pi = 3.14159
module Color = struct
     type t = float * float * float * float
     let create ~a r g b = 
          (r,g,b,a)

     let invis = (0.,0.,0.,0.)
     let black = (0.,0.,0.,1.)
     let white = (1.,1.,1.,1.)

     let red =   (1.,0.,0.,1.)
     let green = (0.,1.,0.,1.)
     let blue =  (0.,0.,1.,1.)

     let to_string (r,g,b,a) = 
          sprintf "<%f,%f,%f,%f>" r g b a

     let defaultbg = black

     let (=) (r1,g1,b1,a1) (r2,g2,b2,a2) = 
          let (=.) x y = Float.abs (x -. y) <. 0.001 in
          if (r1 =. r2) &&
          (g1 =. g2) &&
          (b1 =. b2) &&
          (a1 =. a2) 
          then true
          else
               begin
               List.iter [(r1,r2); (g2,g2); (b1,b2); (a1,a2);]
               ~f:(fun (x,y) -> printf "%f - %f != %f\n" x y (x -. y));
               false
               end
end

module Transform = struct
     module T5 = struct
          type index =  [`i1|`i2|`i3|`i4|`i5]
          let indices = [`i1;`i2;`i3;`i4;`i5]
          type 'a t = 'a*'a*'a*'a*'a

          let get i (x1,x2,x3,x4,x5) = 
               match i with
               | `i1 -> x1
               | `i2 -> x2
               | `i3 -> x3
               | `i4 -> x4
               | `i5 -> x5

          let set i y (x1,x2,x3,x4,x5) = 
               match i with
               | `i1 -> (y,x2,x3,x4,x5)
               | `i2 -> (x1,y,x3,x4,x5)
               | `i3 -> (x1,x2,y,x4,x5)
               | `i4 -> (x1,x2,x3,y,x5)
               | `i5 -> (x1,x2,x3,x4,y)

          let create : 'a -> 'a t = fun v -> (v,v,v,v,v)
     end
     module Row = struct
          type index =  T5.index
          let indices = T5.indices
          type t = float T5.t
          let get = T5.get
          let set = T5.set
          let create : float -> t = T5.create
          let dot (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) =
               x1*.y1 +.
               x2*.y2 +.
               x3*.y3 +.
               x4*.y4 +.
               x5*.y5

          let (=) (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) =
               x1=.y1 &&
               x2=.y2 &&
               x3=.y3 &&
               x4=.y4 &&
               x5=.y5

          let to_string (x1,x2,x3,x4,x5) = 
               sprintf "[%f,%f,%f,%f,%f]"
                    x1 x2 x3 x4 x5
     end
     module Mat = struct
          type index =  T5.index
          let indices = T5.indices
          type t = Row.t T5.t
          let get = T5.get
          let set = T5.set
          let create : Row.t -> t = T5.create
          let (=) (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) =
               Row.(=) x1 y1 &&
               Row.(=) x2 y2 &&
               Row.(=) x3 y3 &&
               Row.(=) x4 y4 &&
               Row.(=) x5 y5

     end
     type t = Mat.t

     let get i j t = 
          Mat.get j (Row.get i t)

     let set i j v t = 
          let row = Row.set j v (Mat.get i t) in
          Mat.set i row t

     let indices = 
          List.fold Row.indices ~init:[] ~f:(fun acc i -> 
               List.fold Row.indices ~init:acc ~f:(fun acc j ->
                    (i,j) :: acc))

     let row = Mat.get
     let col j t = 
          (
               get `i1 j t,
               get `i2 j t,
               get `i3 j t,
               get `i4 j t,
               get `i5 j t
          )

     let initialize (f:(Mat.index * Mat.index) -> float) = 
          let init = Mat.create (Row.create 0.0) in
          List.fold indices ~init
               ~f:(fun acc (i,j) -> set i j (f (i,j)) acc)

     let identity =
          initialize (fun (i,j) -> if i = j then 1.0 else 0.0)

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
               Row.dot (row `i3 t) vec,
               Row.dot (row `i4 t) vec,
               Row.dot (row `i5 t) vec
          )

     let transpose t = 
          initialize (fun (i,j) -> get j i t)

     let to_string t =
          String.concat ~sep:"\n" (List.map Mat.indices ~f:(fun i -> Row.to_string (row i t)))

     let translate v =
          initialize (fun (i,j) ->
               if j = `i5 
               then (get i j identity) +. (Row.get i v)
               else (get i j identity))

     let scale v =
          initialize (fun (i,j) ->
               if i = j 
               then Row.get i v
               else 0.0
          )

     let rotate i j d : t = 
          let d = (d *. pi /. 180.) in
          let ret = identity in
          let ret =  set i i (cos d) ret in
          let ret =  set j j (cos d) ret in
          let ret =  set i j (-. sin d) ret in
          let ret =  set j i (sin d) ret in
          ret

     let eq t1 t2 =
          Row.(=) (row `i1 t1) (row `i1 t2) &&
          Row.(=) (row `i2 t1) (row `i2 t2) &&
          Row.(=) (row `i3 t1) (row `i3 t2) &&
          Row.(=) (row `i4 t1) (row `i4 t2) &&
          Row.(=) (row `i5 t1) (row `i5 t2)

     module Infix_g = struct
          let (=|) = eq
          let ( *|) = multiply
     end


     let apply t (r,g,b,a) = 
          let calc_row i = 
               let (x1,x2,x3,x4,x5) = T5.get i t in
               r*.x1 +. g *. x2 +. b *. x3 +. a *. x4 +. x5
          in (
          (calc_row `i1),
          (calc_row `i2),
          (calc_row `i3),
          (calc_row `i4))

     let of_color (a,b,c,d) = translate (a,b,c,0.0,0.0)
     let color_scale r g b = scale (r,g,b,1.0,1.0)
     let get_color t = 
          let (a,b,c,d,_) = mul_vec t (0.,0.,0.,0.,1.) in
          (a,b,c,d)
     let scalar_mul v = scale (T5.create v)
     let brighten x = scalar_mul x
     let rotate_rg d = rotate `i1 `i2 d
     let rotate_rb d = rotate `i1 `i3 d
     let rotate_gb d = rotate `i2 `i3 d
     let rotate_ra d = rotate `i1 `i4 d
     let rotate_ga d = rotate `i2 `i4 d
     let rotate_ba d = rotate `i3 `i4 d


     let _ = 
     assert ((1.0,0.0,0.0,0.0,0.0) = (row `i1 (
          (1.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0))));

     assert ((1.0,2.0,0.0,0.0,0.0) = (col `i1 (
          (1.0, 0.0, 0.0, 0.0, 0.0),
          (2.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0))))
     (*
     module Row = struct
          type t = float*float*float*float*float
     end


     type t = (Row.t * Row.t * Row.t * Row.t * Row.t)

     let get t i j = 
          let row = T5.get t i in
          T5.get row j

     let set i j x t =
          let row = T5.get t i in
          let new_row = T5.set row j x in
          T5.set t i new_row

     let identity = (
          (1., 0., 0., 0., 0.),
          (0., 1., 0., 0., 0.),
          (0., 0., 1., 0., 0.),
          (0., 0., 0., 1., 0.),
          (0., 0., 0., 0., 1.))

     let apply t (r,g,b,a) = 
          let calc_row i = 
               let (x1,x2,x3,x4,x5) = T5.get t i in
               r*.x1 +. g *. x2 +. b *. x3 +. a *. x4 +. x5
          in (
          (calc_row `i1),
          (calc_row `i2),
          (calc_row `i3),
          (calc_row `i4))

     let mapi t ~f =
          List.fold T5.indices ~init:t
          ~f:(fun acc i ->
               List.fold T5.indices ~init:acc
               ~f:(fun acc j ->
                    let new_val = f (i,j) (get t i j) in
                    set i j new_val acc
               ))

     let mul t1 t2 = 
          let row_col i j = 
               List.fold T5.indices ~init:0.0 ~f:(fun acc x ->
                    acc +. (get t1 i x)  *. (get t2 x j))
          in
          mapi t1 ~f:(fun (i,j) _ -> row_col i j)

     let scalar_mul scalar t = 
          mapi t ~f:(fun _ x -> scalar *. x)


     let rotate i j d = 
          let d = (d *. pi /. 180.) in
          identity
          |! set i i (cos d) 
          |! set j j (cos d) 
          |! set i j (0. -. sin d) 
          |! set j i (sin d)

     let brighten x = scalar_mul x identity 
     let rotate_rg d = rotate `i1 `i2 d
     let rotate_rb d = rotate `i1 `i3 d
     let rotate_gb d = rotate `i2 `i3 d
     let rotate_ra d = rotate `i1 `i4 d
     let rotate_ga d = rotate `i2 `i4 d
     let rotate_ba d = rotate `i3 `i4 d
     *)

end
include Color



let _ = 
     let assert_eqc = assert_eq Color.(=) Color.to_string in
     printf "identity:%s\n\n" (Transform.to_string Transform.identity);
     printf "red:%s\n\n" (Transform.to_string (Transform.of_color Color.red));
     assert (assert_eqc Color.red (Transform.apply Transform.identity Color.red));
     assert (assert_eqc Color.red (Transform.apply (Transform.of_color
     Color.red) Color.black));
     assert (assert_eqc Color.blue (Transform.apply (Transform.rotate_rb (90.))
     Color.red));

     (*;
     printf "getc:%s\n" (Transform.to_string (Transform.of_color Color.red));
     assert (assert_eqc Color.red (Transform.get_color (Transform.of_color
     Color.red)))*)
