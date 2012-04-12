open Core.Std
open Vec.Infix
open Matrix.Infix

module Cfcns = struct
     external render_png : string -> int -> int -> Color.t array -> bool =
          "render_png"
end

let fold_int_range ~init ~f lower upper = 
     let acc = ref init in
     for x = lower to (upper-1) do
          acc := f (!acc) x
     done;
     !acc


let t2_order x1 x2 = if x1 <. x2 then (x1,x2) else (x2,x1)
type t = 
     {
          pixelwidth: int;
          pixelheight: int;
          pixelborder: int;
          base: Matrix.t;
          image: Color.t array;
          alias_amt: int;
     }

exception Found_pixel
exception Invalid_pixel

let set_pixel t (x,y) ~c =
     let loc = y * t.pixelwidth + x in
     if x >= t.pixelwidth || x < 0 || y >= t.pixelheight || y < 0 then
          raise Invalid_pixel
     else
          (*TODO:Alpha blending*)
     t.image.(loc) <- c
     (*;
     if loc = 12944
     then
          raise Found_pixel*)

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
     let x2 = min x2 (t.pixelwidth-1) in
     let y2 = min y2 (t.pixelheight-1) in
     let ibase = Matrix.invert t.base in
     for x = x1 to x2
     do
          for y = y1 to y2
          do
               let tvec = ibase *|$ (Vec.of_ints (x,y)) in
               match f tvec with
               | None -> ()
               | Some c -> 
                         try 
                              set_pixel t (x,y) ~c
                         with
                         | Found_pixel -> 
                                   printf "%d,%d-%d,%d" x1 y1 x2 y2;
                                   printf "%s" 
                                   (Rect.to_string (Rect.apply 
                                   (Vec.of_ints (x1,y1),Vec.of_ints (x2,y2)) ibase));
                                   raise Found_pixel
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
     let border = t.pixelborder * t.alias_amt in
     let borderx = (float_of_int border) /. viewport_width in
     let bordery = (float_of_int border) /. viewport_height in
     let base : Matrix.t= 
          Matrix.scale 
               ((float_of_int (t.pixelwidth - border)) /. viewport_width) 
               ((float_of_int (t.pixelheight - border)) /. viewport_height)
     in
     let base : Matrix.t =
          base *| Matrix.translate (borderx -. x1) (bordery -. y1)
     in
     {t with base}

let create ~pixelwidth ~pixelheight ?(pixelborder=0) ?(bg=Color.defaultbg) ?(alias=3) () =
     assert (alias > 0);
     let pixelwidth  = pixelwidth*alias in
     let pixelheight  = pixelheight*alias in
     let base = 
          (Matrix.scale (float_of_int pixelwidth) (float_of_int pixelheight))
          *| (Matrix.translate (0.5) (0.5))
     in
     let image = Array.create (pixelwidth*pixelheight) bg in
     { pixelwidth; pixelheight; base; image; alias_amt=alias; pixelborder}

let rec row_to_string ?(x=0) ~y t =
     if x >= t.pixelheight 
     then ""
     else
          (sprintf "%s" (Color.to_string (t.image.(y*t.pixelwidth+x)))) ^ 
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



let write_txt t filename =
     let chan = open_out filename in
     fprintf chan "%d,%d\n" t.pixelwidth t.pixelheight;
     Array.iteri t.image ~f:(fun i x ->
          fprintf chan "%s" (Color.to_string x))

let write t filename = 
     let ret = Cfcns.render_png filename t.pixelwidth t.pixelheight t.image in
     if ret
     then printf "success.\n"
     else printf "failure.\n"


let antialias t =
     let aliased =
          create ~pixelwidth:(t.pixelwidth / t.alias_amt) ~pixelheight:(t.pixelheight
          / t.alias_amt) ~alias:1 ~pixelborder:t.pixelborder () 
     in
     for x = 0 to aliased.pixelwidth-1 do
          for y = 0 to aliased.pixelheight-1 do
               let (sumr,sumg,sumb,suma) = 
                    fold_int_range 0 t.alias_amt ~init:(Color.black) ~f:(fun init ax ->
                         fold_int_range 0 t.alias_amt ~init ~f:(fun
                              (accr,accg,accb,acca) ay ->
                              let ax = x * t.alias_amt + ax in
                              let ay = y * t.alias_amt + ay in
                              let (srcr,srcg,srcb,srca) =
                                   t.image.(ay*t.pixelwidth+ax)
                              in
                              (*TODO:alpha compositing*)
                              (accr+.srcr, accg+.srcg, accb+.srcb, acca *.
                              srca)))
               in
               let div = float_of_int (t.alias_amt * t.alias_amt) in
               aliased.image.(y*aliased.pixelwidth+x) <-
                    (
                         (sumr /. div),
                         (sumg /. div),
                         (sumb /. div),
                         (suma /. div)
                    )
          done
     done;
     aliased

