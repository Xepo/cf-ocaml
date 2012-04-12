open Core.Std
open Vec.Infix
open Matrix.Infix

let apply settings r =
     Renderable.apply_settings r settings

let apply_to_list settings =
     List.map ~f:(apply settings)

module Operations = struct
     let shape ?(trace="") f = 
          [Renderable.of_fcn ~trace Settings.identity f]
     let circle = 
          [Renderable.create_basic Settings.identity Basic_shape.Circle]
     let square = 
          [Renderable.create_basic Settings.identity Basic_shape.Square]
     let add_trace trace s =
          Renderable.add_trace trace s

     let ( *- ) r settings = apply_to_list settings r
     let ( ++ ) f1 f2 = List.rev_append f1 f2

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

end



let print_shapes = 
     List.iter ~f:(fun r ->
          printf "%s\n" (Renderable.to_string r))

let render_shapes output = (List.iter ~f:(fun t -> Renderable.render output t))

let render_scene ~w:pixelwidth ~h:pixelheight ?(alias=5) ?bg ~filename s =
     Random.self_init ();
     (*TODO:Should use after alias w and h*)
     let world = World.add_list World.empty (s ()) in
     let world = World.expand_until_size ~pixelwidth ~pixelheight world in
     let output = Outputtable.create ~pixelwidth ~pixelheight ~alias
     ~pixelborder:0 ?bg () in
     let extents = Option.value_exn world.World.extents in
     let output = Outputtable.new_viewport output extents in
     (*print_shapes world.World.basics;*)
     render_shapes output world.World.basics;
     printf "%s\n" (Outputtable.string_of_viewport output);

     printf "Writing...\n";
     let aliased = Outputtable.antialias output in
     Outputtable.write aliased filename;
     printf "Done rendering %d shapes\n" (world.World.basic_count)

include Settings.Operations
include Operations
