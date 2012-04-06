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
let tra ?(x=0.0) ?(y=0.0) ?s ?w ?h ?(rot=0.0) () =
     let w,h =
          match s,w,h with
          | Some _, _, Some _ 
          | Some _, Some _, _
               -> failwith "Cannot apply tra with both s and w,h\n"
          | Some s, _, _ -> s,s
          | None, _, _ -> Option.value ~default:1.0 w, Option.value ~default:1.0 h
     in
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



module World = struct
     type t = 
          {
               basics: Renderable.t list;
               fcns: Renderable.t list Float.Map.t;
               basic_count : int;
               extents : Rect.t option;
          }

     let empty = {basics=[]; fcns=Float.Map.empty; basic_count=0; extents=None}

     let add_to_map map x = 
          let size = Context.size x.Renderable.context in
          Float.Map.change map size
          (function
               | None -> Some [x]
               | Some l -> Some (x :: l))

     let rec pop_from_map map =
          match Float.Map.max_elt map with
          | None -> (map, None)
          | Some (key, []) -> 
                    pop_from_map (Float.Map.remove map key)
          | Some (key,ret :: data) ->
               (Float.Map.add map ~key ~data,
                    Some ret)

     let largest_size t =
          match Float.Map.max_elt t.fcns with
          | None -> -.1.0
          | Some (key,_) -> key

     let add_basic t x =
          if 0 = ((t.basic_count+1) mod 1000)
          then 
               printf 
                    "Expanded %d shapes, largest size %f\n" 
                    (t.basic_count+1)
                    (largest_size t);
          let xextent = Rect.expand_unit x.Renderable.context in
          let extents = 
               match t.extents with
               | None -> Some xextent
               | Some extents -> Some (Rect.expand extents xextent)
          in
          {t with basics=x :: t.basics; basic_count=t.basic_count+1; extents;}

     let add t x =
          if Renderable.is_basic x 
          then add_basic t x
          else {t with fcns=add_to_map t.fcns x}

     let add_list t l =
          List.fold l ~init:t ~f:(fun acc x ->
               add acc x)

     let pop_fcn t = 
          let fcns, ret = 
               pop_from_map t.fcns 
          in
          ({t with fcns}, ret)
end

let calculate_threshold ~pixelwidth ~pixelheight world = 
     let extents = Option.value_exn world.World.extents in
     min 
          ((Rect.width extents) /. (float_of_int pixelwidth))
          ((Rect.height extents) /. (float_of_int pixelheight))

let rec expand_until_size ~pixelwidth ~pixelheight world =
     let rec recurs ~size_threshold rec_amt world =
          if rec_amt <= 0
          then 
               (printf "Hit rec limit\n";
               world)
          else
               let (world, this) = World.pop_fcn world in
               match this with
               | None -> world
               | Some this ->
                    let size_threshold =
                         if world.World.basic_count > 0
                         then calculate_threshold ~pixelwidth ~pixelheight world 
                         else size_threshold
                    in
                    if world.World.basic_count > 0 &&
                         Context.size this.Renderable.context <. size_threshold
                    then World.add world this
                    else
                         let expanded = Renderable.expand this in
                         let world = 
                              List.fold expanded ~init:world ~f:World.add
                         in
                         recurs ~size_threshold (rec_amt - 1) world
     in
     recurs ~size_threshold:100000.0 20000 world

let print_shapes = 
     List.iter ~f:(fun r ->
          printf "%s\n" (Renderable.to_string r))

let render_shapes output = (List.iter ~f:(fun t -> Renderable.render output t))

let render_scene ~w:pixelwidth ~h:pixelheight ?(alias=5) ?bg s =
     Random.self_init ();
     (*TODO:Should use after alias w and h*)
     let world = World.add_list World.empty (s ()) in
     let world = expand_until_size ~pixelwidth ~pixelheight world in
     let output = Outputtable.create ~pixelwidth ~pixelheight ~alias ?bg () in
     let extents = Option.value_exn world.World.extents in
     let output = Outputtable.new_viewport output extents in
     render_shapes output world.World.basics;
     printf "%s\n" (Outputtable.string_of_viewport output);

     let aliased = Outputtable.antialias output in
     Outputtable.write_arr aliased "image.in";
     printf "Done rendering %d shapes\n" (world.World.basic_count)
