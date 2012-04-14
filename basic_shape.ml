open Core.Std

type t = 
     | Circle
     | Square

let to_string = function
     | Circle -> "circle"
     | Square -> "square"

let render output settings t : unit =
     let c = Settings.get_color settings in
     match t with
     | Square ->
          Settings.iterate_unit_rect output settings ~f:(fun (_,_) -> Some c)
     | Circle ->
          Settings.iterate_unit_rect output settings 
          ~f:(fun (x,y) -> 
               if (((x *. x) +. (y *. y)) <. 0.25)
               then Some c
               else None)
