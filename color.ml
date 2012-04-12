open Core.Std

let pi = 3.14159
module Color = struct
     type t = float * float * float * float
     let create ~r ~g ~b ~a = 
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
include Color

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

          let create v = (v,v,v,v,v)
     end


     include Generic_matrix.Matrix(T5)

     let apply t (r,g,b,a) = 
          let calc_row i = 
               let (x1,x2,x3,x4,x5) = T5.get i t in
               r*.x1 +. g *. x2 +. b *. x3 +. a *. x4 +. x5
          in (
          (calc_row `i1),
          (calc_row `i2),
          (calc_row `i3),
          (calc_row `i4))

     let of_color (a,b,c,d) = translate (a,b,c,d,1.0)
     let get_color t = 
          let (a,b,c,d,_) = mul_vec t (0.,0.,0.,0.,0.) in
          (a,b,c,d)
     let scalar_mul v = scale (T5.create v)
     let brighten x = scalar_mul x
     let rotate_rg d = rotate `i1 `i2 d
     let rotate_rb d = rotate `i1 `i3 d
     let rotate_gb d = rotate `i2 `i3 d
     let rotate_ra d = rotate `i1 `i4 d
     let rotate_ga d = rotate `i2 `i4 d
     let rotate_ba d = rotate `i3 `i4 d
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



let assert_eq eq to_str x y = 
     if eq x y 
     then ()
     else
          printf "Assert failure: %s vs. %s\n" (to_str x) (to_str y)

let _ = 
     let assert_eqc = assert_eq Color.(=) Color.to_string in
     assert_eqc Color.red (Transform.apply Transform.identity Color.red);
     assert_eqc Color.blue (Transform.apply (Transform.rotate_rb (90.)) Color.red)
