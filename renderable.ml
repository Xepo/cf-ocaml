open Core.Std
open Vec.Infix
open Matrix.Infix

module Shapes = struct
     type 'a t = 
     | Basic of Basic_shape.t
     | Fcn of (unit -> 'a list)
end


type t = 
     {
          settings : Settings.t;
          shape : t Shapes.t;
          trace : string;
     }

let apply_matrix t = 
     Settings.apply_matrix t.settings
let apply_matrix_rev t = 
     Settings.apply_matrix_rev t.settings

let apply_settings t1 settings =
     {t1 with settings=Settings.combine t1.settings settings}

let add_trace str = 
     List.map ~f:(fun r -> {r with trace=str ^ "(" ^ r.trace ^ ")"})

let rec expand t = 
     match t.shape with
     | Shapes.Basic _ -> [t]
     | Shapes.Fcn f ->
          add_trace t.trace 
          (
               List.fold (f ()) ~init:[] 
                    ~f:(fun acc new_t ->
                         let new_t = apply_settings new_t t.settings in
                         new_t :: acc)
          )


let rec render output t = 
     match t.shape with
     | Shapes.Basic basic ->
               begin try
                    (Basic_shape.render output t.settings basic)
               with
                    | Outputtable.Found_pixel -> printf "!!!!found_pixel:%s %s\n" t.trace
                    (Settings.to_string t.settings)
               end
     | Shapes.Fcn f -> ()

let create_basic settings shape =
     { settings; shape=Shapes.Basic shape;
     trace=Basic_shape.to_string shape}
let of_fcn ~trace settings fcn =
     { settings; shape=Shapes.Fcn fcn; trace}
let is_basic t =
     match t.shape with
     | Shapes.Basic _ -> true
     | _ -> false

let to_string t = 
     match t.shape with
     | Shapes.Basic b -> 
               sprintf "basic(%s:%s)" 
               (Basic_shape.to_string b) 
               (Settings.to_string t.settings) 
     | Shapes.Fcn _ -> "f"
