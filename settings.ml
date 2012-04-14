open Core.Std
open Vec.Infix
open Matrix.Infix

type t = 
     { 
          value: Pixel.t option;
          transform: Matrix.t; 
     }

let identity = {value=None; transform=Matrix.identity}

let to_string t =
     sprintf "%s -> %s" 
          (Option.value_map t.value
               ~default:"N"
               ~f:Pixel.to_string)
          (Matrix.to_string t.transform)

let value t = Option.value ~default:0 t.value

let of_matrix m = 
     {
          value=None;
          transform=m;
     }

let of_value v = 
     {
          value=Some v;
          transform=Matrix.identity;
     }

let add_value i1 i2 = 
     match i1,i2 with
     | None,None -> None
     | Some x,Some y -> Some (x + y)
     | Some x,_
     | _,Some x -> Some x

let combine t1 t2 =
     {
          value=add_value t1.value t2.value;
          transform=t2.transform*|t1.transform;
     }

let apply_matrix t matrix =
     { t with transform = matrix *| t.transform; }
let apply_matrix_rev t matrix = 
     { t with transform = t.transform *| matrix; }

let translate x y = of_matrix (Matrix.translate x y)
let scale x y = of_matrix (Matrix.scale x y)
let rotate d = of_matrix (Matrix.rotate d)

let iterate_unit_rect ~f output t = 
     let rc = Rect.containing_rect_of_unit t.transform in
     let it = Matrix.invert t.transform in
     Outputtable.iter rc output ~f:(fun vec ->
          let tvec = it *|$ vec in
          if Vec.in_unit tvec
          then 
               f tvec
          else
               None
     )


let size_of_unit t =
     let (cxx,cxy,_) = (Matrix.row `i1 t.transform) in
     let (cyx,cyy,_) = (Matrix.row `i2 t.transform) in
     let ( !! ) f = if f > 0.0 then f else 0. -. f in
     min ((!!cxx) +. (!!cxy)) ((!!cyx) +. (!!cyy))

let containing_rect t =
     Rect.containing_rect_of_unit t.transform


module Operations = struct 
     let flip_y = of_matrix (Matrix.create (1.,0.,0.) (0.,-1.,0.) (0.,0.,1.))
     let tr = translate
     let sc = scale
     let rot = rotate
     let v = of_value
     let tra ?(x=0.0) ?(y=0.0) ?s ?w ?h ?(rot=0.0) () =
          let w,h =
               match s,w,h with
               | Some _, _, Some _ 
               | Some _, Some _, _
                    -> failwith "Cannot apply tra with both s and w,h\n"
               | Some s, _, _ -> s,s
               | None, _, _ -> Option.value ~default:1.0 w, Option.value ~default:1.0 h
          in
          of_matrix
          (Matrix.translate x y
          *| Matrix.rotate rot
          *| Matrix.scale w h)

     let ( +| ) = combine
end
