let _ = print_string "beg\n"
open Core.Std
;;

module Pixel = struct
     type t = int
end

let pi = 3.14159
let t2_order x1 x2 = if x1 <. x2 then (x1,x2) else (x2,x1)
module Vec = struct
     type t = float * float
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
     let ( =$ ) ((x1,y1):t) ((x2,y2):t) : bool= ((Float.(=) x1 x2) && (Float.(=) y1 y2))

     let to_string (x,y) = sprintf "(%f,%f)" x y
     let assert_equal v1 v2 =
          if not (v1 =$ v2)
          then begin
               printf "ASSERT FAILED: %s != %s" (to_string v1) (to_string v2);
               assert (v1 =$ v2);
          end else
               assert (v1 =$ v2)
end
let (=$) = Vec.(=$)

let test () =
     assert (Vec.in_unit (0.4,0.4));
     assert (not (Vec.in_unit (50.4,0.4)));
     assert (not (Vec.in_unit (0.4,50.4)))


module Matrix = struct
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

end

let ( *|$ ) = Matrix.( *|$ )
let ( *| ) = Matrix.( *|)
let ( =| ) = Matrix.( =|)

module Rect = struct
     type t = Vec.t * Vec.t

     let expand r (v1,v2) =
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
end
let _ = 
     printf "rotted:%s\n\n" (Vec.to_string ((Matrix.rotate 45.) *|$ (3.,2.)))

let test () = 
     let v:Vec.t = (5.0,5.0) in
     let m = Matrix.identity in
     let r:Vec.t = m *|$ v in
     assert (r =$ v);
     (*printf "%s *| %s = %s" (Matrix.to_string m) (Matrix.to_string m)
      * (Matrix.to_string (m *| m));*)
     assert ((m *| m) =| m);

     let m1 = (Matrix.translate 5. 1.) *| (Matrix.translate 10. 2.) in
     let m2 = (Matrix.translate 15. 3.) in
     assert (m1 =| m2);
     assert ((m1 *|$ v) =$ (m2 *|$ v));


     let v:Vec.t = (5.0,-1.0) in
     let m1 = Matrix.translate 5. 1. in
     assert ((m1 *|$ v) =$ (10.0,0.0));

     let v:Vec.t = (5.0,-1.0) in
     let m1 = Matrix.scale 2. 3. in
     assert ((m1 *|$ v) =$ (10.0,-3.0));

     assert ((Matrix.invert Matrix.identity) =| Matrix.identity);
     assert ((Matrix.invert (Matrix.invert m1)) =| m1);
     assert ((Matrix.invert (Matrix.invert (m1*|m2))) =| (m1*|m2));
     let m1 = Matrix.scale 2. 4. in
     let m2 = Matrix.scale 0.5 0.25 in
     assert ((Matrix.invert m1) =| m2);

     let sc = Matrix.scale 2. 4. in
     let tr = Matrix.translate 10. 5. in
     let m = sc *| tr in
     Vec.assert_equal 
     ((m *|$ (0.,0.))) (20.,20.);

     let v = (30.,-30.) in
     let m = Matrix.translate 10. 5. in
     let m2 = Matrix.invert m in
     let v2 = m2 *|$ v in
     Vec.assert_equal v2 (20.,-35.)



let fold_int_range ~init ~f lower upper = 
     let acc = ref init in
     for x = lower to (upper-1) do
          acc := f (!acc) x
     done;
     !acc

module Outputtable = struct
     type t = 
          {
               pixelwidth: int;
               pixelheight: int;
               base: Matrix.t;
               image: Pixel.t array;
               alias_amt: int;
          }

     let set_pixel t (x,y) ~v =
          t.image.(y * t.pixelwidth + x) <- v

     let pixel_to_space t (x,y) =
          let ibase = Matrix.invert t.base in
          ibase *|$ (Vec.of_ints (x,y))


     let iter ((x1,y1),(x2,y2)) ~f t =
          let (x1,x2) = t2_order x1 x2 in
          let (y1,y2) = t2_order y1 y2 in
          let (x1,y1) = t.base *|$ (x1,y1) in
          let (x2,y2) = t.base *|$ (x2,y2) in
          let x1 = int_of_float (x1-.1.) in
          let y1 = int_of_float (y1-.1.) in
          let x2 = int_of_float (x2+.1.) in
          let y2 = int_of_float (y2+.1.) in
          let x1 = max x1 0 in
          let y1 = max y1 0 in
          let x2 = min x2 t.pixelwidth in
          let y2 = min y2 t.pixelheight in
          let ibase = Matrix.invert t.base in
          for x = x1 to x2
          do
               for y = y1 to y2
               do
                    let tvec = ibase *|$ (Vec.of_ints (x,y)) in
                    match f tvec with
                    | None -> ()
                    | Some v -> set_pixel t (x,y) ~v
               done
          done

     let new_viewport t ((x1,y1),(x2,y2)) =
          let (x1,x2) = t2_order x1 x2 in
          let (y1,y2) = t2_order y1 y2 in
          let ratio = (float_of_int t.pixelwidth) /. (float_of_int t.pixelheight) in
          let viewport_width = x2 -. x1 in
          let viewport_height = y2 -. y1 in
          let viewport_width = max (ratio *. viewport_height) viewport_width in
          let viewport_height = max (viewport_width /. ratio) viewport_height in
          (*Gotta keep same ratio as pixels*)
          let base = 
               Matrix.scale 
                    ((float_of_int t.pixelwidth) /. viewport_width) 
                    ((float_of_int t.pixelheight) /. viewport_height)
               *| Matrix.translate (0. -. x1) (0. -. y1)
          in
          {t with base}

     let create ~pixelwidth ~pixelheight ?(alias=3) () =
          assert (alias > 0);
          let pixelwidth  = pixelwidth*alias in
          let pixelheight  = pixelheight*alias in
          let base = 
               (Matrix.scale (float_of_int pixelwidth) (float_of_int pixelheight))
               *| (Matrix.translate (0.5) (0.5))
          in
          let image = Array.create (pixelwidth*pixelheight) 0 in
          { pixelwidth; pixelheight; base; image; alias_amt=alias;}

     let rec row_to_string ?(x=0) ~y t =
          if x >= t.pixelheight 
          then ""
          else
               (sprintf "%d" (t.image.(y*t.pixelwidth+x))) ^ 
                    (row_to_string ~x:(x+1) ~y t)

     let rec to_string_all ?(y=0) t =
          if y >= t.pixelwidth
          then ""
          else
               (row_to_string t ~y) ^ "\n" ^ (to_string_all t ~y:(y+1))

     let string_of_viewport t = 
          let ibase = Matrix.invert t.base in
          let (lx,ly) = ibase *|$ (0.,0.) in
          let (hx,hy) = 
               ibase *|$ (float_of_int t.pixelwidth, float_of_int t.pixelheight)
          in
          sprintf "Viewport:%f %f %f %f\n" lx ly hx hy

     let to_string t =
          let ibase = Matrix.invert t.base in
          let (lx,ly) = ibase *|$ (0.,0.) in
          let (hx,hy) = 
               ibase *|$ (float_of_int t.pixelwidth, float_of_int t.pixelheight)
          in
          sprintf "Viewport:%f %f %f %f\n%s\n" lx ly hx hy (to_string_all t)


     let write t filename =
          let chan = open_out filename in
          fprintf chan "%d,%d\n" t.pixelwidth t.pixelheight;
          for x = 0 to (t.pixelwidth-1)
          do
               for y = 0 to (t.pixelheight-1)
               do
                    fprintf chan "%d" t.image.(t.pixelwidth*y+x)
               done;
               fprintf chan "\n"
          done


     let antialias t =
          let aliased =
               create ~pixelwidth:(t.pixelwidth / t.alias_amt) ~pixelheight:(t.pixelheight
               / t.alias_amt) ~alias:1 () 
          in
          for x = 0 to aliased.pixelwidth do
               for y = 0 to aliased.pixelheight do
                    let sum = 
                         fold_int_range 0 t.alias_amt ~init:0 ~f:(fun init ax ->
                              fold_int_range 0 t.alias_amt ~init ~f:(fun acc ay ->
                                   let ax = x * t.alias_amt + ax in
                                   let ay = y * t.alias_amt + ay in
                                   acc + t.image.(ay*t.pixelwidth+ax)))
                    in
                    aliased.image.(y*aliased.pixelwidth+x) <-
                         (sum / t.alias_amt / t.alias_amt)
               done
          done;
          aliased

end
let test () = 
     let output = Outputtable.create ~pixelwidth:20 ~pixelheight:20 () in
     let output = Outputtable.new_viewport output (((-5.),(-5.)),(5.,5.)) in
     let (x1,y1) = (Outputtable.pixel_to_space output (0,0)) in
     let (x2,y2) = (Outputtable.pixel_to_space output
     (output.Outputtable.pixelwidth,output.Outputtable.pixelheight)) in
     (printf "pixelviewport:%f,%f %f,%f\n" x1 y1 x2 y2)
     ;;

module Context = struct
     type t = Matrix.t

     let iterate_unit ~f output t = 
          let rc = Rect.expand_unit t in
          printf " con:%s\n%s\n\n" (Rect.to_string rc) (Matrix.to_string t);
          let it = Matrix.invert t in
          Outputtable.iter rc output ~f:(fun vec ->
               let tvec = it *|$ vec in
               if Vec.in_unit tvec
               then 
                    f tvec
               else
                    None
          )
end

module Basic_shape = struct
     type t = 
          | Circle
          | Square

     let render : v:Pixel.t -> Outputtable.t ->  Context.t -> t -> unit = 
          fun ~v ->
               let some_v = Some v in
               fun output context t -> match t with
               | Square ->
                    Context.iterate_unit output context ~f:(fun (x,y) -> some_v)
               | Circle ->
                    Context.iterate_unit output context 
                    ~f:(fun (x,y) -> 
                         if (((x *. x) +. (y *. y)) <. 0.25)
                         then 
                              some_v
                         else None)

end

module Renderable = struct
     module Shapes = struct
          type 'a t = 
          | Basic of Basic_shape.t
          | Fcn of (unit -> 'a list)
     end
     type t = 
          {
               value : Pixel.t;
               context : Context.t;
               shape : t Shapes.t;
          }

     let apply_context t context = 
          { t with context = context *| t.context; }

     let rec expand t = 
          match t.shape with
          | Shapes.Basic _ -> [t]
          | Shapes.Fcn f ->
               List.fold (f ()) ~init:[] 
               ~f:(fun acc new_t ->
                    let new_t = apply_context new_t t.context in
                    new_t :: acc)

     let rec render output t = 
          match t.shape with
          | Shapes.Basic basic ->
                    printf "Rendering basic...\n";
                    (Basic_shape.render ~v:t.value output t.context basic)
          | Shapes.Fcn f -> ()

     let create_basic ~v context shape =
          { value=v; context; shape=Shapes.Basic shape; }
     let of_fcn context fcn =
          { value=0; context; shape=Shapes.Fcn fcn; }
     let is_basic t =
          match t.shape with
          | Shapes.Basic _ -> true
          | _ -> false
end

module Scene = struct

     let apply_value v r = {r with Renderable.value=v}
     let apply_context c r = Renderable.apply_context r c

     let shape f = 
          [Renderable.of_fcn Matrix.identity f]
     let circle = 
          [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Circle]
     let square = 
          [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Square]

     let apply_list lr f = 
          List.map lr ~f

     let ( + ) f tr = apply_list f tr
     let ( ++ ) f1 f2 = f1 @ f2

     let tr x y = apply_context (Matrix.translate x y)
     let sc x y = apply_context (Matrix.scale x y)
     let rot d = apply_context (Matrix.rotate d)
     let v v = apply_value v 
     let ( !! ) v = shape
     let ( >>= ) _ f = shape f

     let t () =
          square
     let rec f () =
          circle + (sc 4. 4.) + (tr 2. 4.) + (v 9) ++ 
          circle + (tr (-2.) (-4.)) + (v 9) 
          ++ shape f   + (rot 45.)+ (tr (-5.) (-5.)) + (sc 0.5 0.5)
     let f () =
          square + (v 9) + (rot 45.)



end

let _ = 
     let m = Matrix.invert (Matrix.rotate 45.) in
     let f (x1,y1) =
          let (x,y) = m *|$ (x1,y1) in
          printf "%f,%f=%f,%f\n" x1 y1 x y
     in
     printf "%s" (Matrix.to_string m);
     f (1.,1.);
     f (1.,2.);
     f (2.,1.);
     f (2.,2.)



let main () =
     printf "init\n";
     let output = Outputtable.create ~pixelwidth:(700) ~pixelheight:(700) ~alias:3 () in
     let ex l = List.concat (List.map l ~f:Renderable.expand) in
     let shapes = ex (Scene.f ()) |! ex |! ex |! ex in
     (*

     let context = (Matrix.scale 0.75 0.75) *| (Matrix.translate 1. 1.) in
     let context2 = (Matrix.scale 0.3 0.75) *| (Matrix.translate (0.-. 5.) 1.) in
let shapes = 
          [
               Renderable.create_basic ~v:9 Matrix.identity Basic_shape.Circle ;
               Renderable.create_basic ~v:2 context Basic_shape.Circle ;
               Renderable.create_basic ~v:2 context2 Basic_shape.Square ;
          ]
     in*)
     let rc = List.fold shapes ~init:None ~f:(fun
          rc shape -> 
               if Renderable.is_basic shape 
               then 
                    let mat = shape.Renderable.context in
                    match rc with
                    | None -> Some (Rect.expand_unit mat)
                    | Some rc -> Some (Rect.expand rc (Rect.expand_unit mat))
               else
                    rc)
     in
     let rc = Option.value ~default:Rect.unit_rect rc in
     let output = Outputtable.new_viewport output rc in
     let () =
          List.iter shapes 
          ~f:(fun t -> Renderable.render output t)
     in
     printf "%s\n" (Outputtable.string_of_viewport output);
     let aliased = Outputtable.antialias output in
     Outputtable.write aliased "image.in";
     printf "Done\n"
     (*printf "%s\n" (Outputtable.to_string output)*)

let _ = main ()

