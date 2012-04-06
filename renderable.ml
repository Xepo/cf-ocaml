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
          value : Pixel.t;
          context : Context.t;
          shape : t Shapes.t;
          trace : string;
     }

let apply_context t context = 
     { t with context = context *| t.context; }
let apply_context_rev t context = 
     { t with context = t.context *| context; }

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
                         let new_t = apply_context new_t t.context in
                         new_t :: acc)
          )


let rec render output t = 
     match t.shape with
     | Shapes.Basic basic ->
               begin try
                    (Basic_shape.render ~v:t.value output t.context basic)
               with
                    | Outputtable.Found_pixel -> printf "!!!!found_pixel:%s %s\n" t.trace
                    (Matrix.to_string t.context)
               end
     | Shapes.Fcn f -> ()

let create_basic ~v context shape =
     { value=v; context; shape=Shapes.Basic shape;
     trace=Basic_shape.to_string shape}
let of_fcn ~trace context fcn =
     { value=0; context; shape=Shapes.Fcn fcn; trace}
let is_basic t =
     match t.shape with
     | Shapes.Basic _ -> true
     | _ -> false

let to_string t = 
     match t.shape with
     | Shapes.Basic b -> 
               sprintf "basic(%s:%d:%s)" 
               (Basic_shape.to_string b) 
               t.value
               (Matrix.to_string t.context) 
     | Shapes.Fcn _ -> "f"
