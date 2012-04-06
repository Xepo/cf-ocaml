open Core.Std
open Vec.Infix
open Matrix.Infix

module Settings = struct
     type t = 
          { value: int option; transform: Matrix.t; }
     let of_matrix m = {value=None; transform=m}
     let of_value v = {value=Some v; transform=Matrix.identity}

     let translate x y = of_matrix (Matrix.translate x y)
     let scale x y = of_matrix (Matrix.scale x y)
     let rotate d = of_matrix (Matrix.rotate d)
     let flip_y = of_matrix (Matrix.create (1.,0.,0.) (0.,-1.,0.) (0.,0.,1.))
     let value = of_value

     let combine t1 t2 =
          {value=t2.value; transform=t2.transform*|t1.transform;}
     let apply t r =
          let r = Renderable.apply_context r t.transform in
          {r with 
          Renderable.value=Option.value ~default:r.Renderable.value t.value
          }
     let apply_rev t r =
          let r = Renderable.apply_context_rev r t.transform in
          {r with 
          Renderable.value=Option.value ~default:r.Renderable.value t.value
          }
end

let ( +| ) = Settings.combine
let apply_to_list settings =
     List.map ~f:(Settings.apply settings)
let apply_to_list_rev settings =
     List.map ~f:(Settings.apply_rev settings)

let flip_y = Settings.flip_y
let shape ?(trace="") f = 
     [Renderable.of_fcn ~trace Matrix.identity f]
let circle = 
     [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Circle]
let square = 
     [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Square]

let add_trace trace s =
     Renderable.add_trace trace s

let ( *+ ) r settings = apply_to_list_rev settings r
let ( *- ) r settings = apply_to_list settings r
let ( ++ ) f1 f2 = List.rev_append f1 f2

let tr = Settings.translate
let sc = Settings.scale
let rot = Settings.rotate
let v = Settings.value
let tra ?(x=0.0) ?(y=0.0) ?(w=1.0) ?(h=1.0) ?(rot=0.0) () =
     Settings.of_matrix
     (Matrix.translate x y
     *| Matrix.rotate rot
     *| Matrix.scale w h)






let rec many n t s =
     let rec many n thist s () = 
          if n > 0
          then 
               (s *- thist) ++
               many (n-1) (Settings.combine thist t) s ()
          else
               []
     in
     shape (many n t s)

let choose l = 
     let total = 
          List.fold l ~init:(0.0) ~f:(fun acc (prob, _) -> acc +. prob)
     in
     fun () ->
          let c = Random.float total in
          let (_, r) = 
          List.fold l ~init:(c, None)
          ~f:(fun (c, chosen) (p, r) ->
               if c >= 0. then
                    (c -. p, Some r)
               else
                    (-1.0, chosen))
          in
          Option.value ~default:[] r



     
let split_fcns_basics =
     List.fold ~init:([],[]) ~f:(fun (basics, fcns) s ->
          if Renderable.is_basic s 
          then ((s :: basics), fcns)
          else (basics, (s :: fcns)))

let size_threshold = 0.0005
let last_amount_of_shapes = ref (-1)

let add_shape_count amount_of_shapes inc =
     amount_of_shapes := !amount_of_shapes + inc;
     if ((!amount_of_shapes) - (!last_amount_of_shapes)) >= 500 
     then
          begin
          last_amount_of_shapes := !amount_of_shapes;
          printf "Rendered %d shapes\n" !amount_of_shapes
          end
     else ()

let rec expand_until fcn_list amount_of_shapes =
     let push map x =
          let size = Context.size x.Renderable.context in
          if size <= size_threshold
          then 
               map
          else
               Float.Map.change map size
               (function
                    | None -> Some [x]
                    | Some l -> Some (x :: l))
     in
     let rec pop map =
          match Float.Map.max_elt map with
          | None -> 
                    printf "empty!\n";
                    (map, None)
          | Some (key, []) -> 
                    pop (Float.Map.remove map key)
          | Some (key,ret :: data) ->
               (Float.Map.change map key
               (function
                    | None 
                    | Some [] -> None
                    | Some (_ :: l) -> Some l),
                    Some ret)
     in
     let rec recurs rec_amt (basics,fcns) =
          if rec_amt <= 0
          then 
               (printf "Hit rec limit\n";
               (basics, fcns))
          else
               let (fcns, this) = pop fcns in
               match this with
               | None -> (basics, fcns)
               | Some this ->
                    let expanded = Renderable.expand this in
                    let (newbasics,newfcns) = split_fcns_basics expanded in
                    add_shape_count amount_of_shapes (List.length newbasics);
                    let basics = List.rev_append newbasics basics in
                    let fcns = 
                         List.fold newfcns ~init:fcns
                              ~f:(push)
                    in
                    recurs (rec_amt - 1) (basics,fcns)
     in
     let fcns = 
          List.fold fcn_list ~init:Float.Map.empty 
          ~f:(push)
     in
     
     let (ret, _) = recurs 50000 ([], fcns) in
     ret


let find_extents shapes = 
     let rc = 
          match shapes with
          | x :: _ -> Rect.expand_unit x.Renderable.context
          | [] -> failwith "No shapes generated"
     in
     let rc = List.fold shapes ~init:rc 
          ~f:(fun rc shape -> 
               let mat = shape.Renderable.context in
               Rect.expand rc (Rect.expand_unit mat))
     in
     rc

let print_shapes = 
     List.iter ~f:(fun r ->
          printf "%s\n" (Renderable.to_string r))

let render_shapes output = (List.iter ~f:(fun t -> Renderable.render output t))

let render_scene ~w:pixelwidth ~h:pixelheight ?(alias=5) s =
     Random.self_init ();
     let amount_of_shapes = ref 0 in
     let shapes = expand_until (s ()) amount_of_shapes in
     let output = Outputtable.create ~pixelwidth ~pixelheight ~alias () in
     let output = Outputtable.new_viewport output (find_extents shapes) in
     render_shapes output shapes;
     printf "%s\n" (Outputtable.string_of_viewport output);

     let aliased = Outputtable.antialias output in
     Outputtable.write_arr aliased "image.in";
     printf "Done rendering %d shapes\n" !amount_of_shapes
