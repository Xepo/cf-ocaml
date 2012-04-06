open Core.Std

type t = 
     | Circle
     | Square

let to_string = function
     | Circle -> "circle"
     | Square -> "square"

let render output settings t : unit =
     let v = Settings.value settings in
     match t with
     | Square ->
          Settings.iterate_unit_rect output settings ~f:(fun (_,_) -> Some v)
     | Circle ->
          Settings.iterate_unit_rect output settings 
          ~f:(fun (x,y) -> 
               if (((x *. x) +. (y *. y)) <. 0.25)
               then Some v
               else None)
