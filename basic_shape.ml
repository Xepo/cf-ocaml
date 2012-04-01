open Core.Std

type t = 
     | Circle
     | Square

let to_string = function
     | Circle -> "circle"
     | Square -> "square"

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
