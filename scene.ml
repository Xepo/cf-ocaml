open Core.Std

let (=$) = Vec.(=$)
let ( *|$ ) = Matrix.( *|$ )
let ( *| ) = Matrix.( *|)
let ( =| ) = Matrix.( =|)

module Settings = struct
     type settings = 
          { value: int option; transform: Matrix.t; }
     let of_matrix m = {value=None; transform=m}
     let of_value v = {value=Some v; transform=Matrix.identity}

     let translate x y = of_matrix (Matrix.translate x y)
     let scale x y = of_matrix (Matrix.scale x y)
     let rotate d = of_matrix (Matrix.rotate d)
     let value = of_value

     let combine t1 t2 =
          {value=t2.value; transform=t1.transform*|t2.transform;}
     let apply t r =
          let r = Renderable.apply_context r t.transform in
          {r with 
          Renderable.value=Option.value ~default:r.Renderable.value t.value
          }
end

let apply_to_list settings =
     List.map ~f:(Settings.apply settings)

let shape ?(trace="") f = 
     [Renderable.of_fcn ~trace Matrix.identity f]
let circle = 
     [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Circle]
let square = 
     [Renderable.create_basic ~v:0 Matrix.identity Basic_shape.Square]

let add_trace trace s =
     Renderable.add_trace trace s

let ( + ) r settings = apply_to_list settings r
let ( ++ ) f1 f2 = f1 @ f2

let tr = Settings.translate
let sc = Settings.scale
let rot = Settings.rotate
let v = Settings.value


let rec many n t s =
     let rec many n thist s () = 
          if n > 0
          then 
               (s + thist) ++
               many (n-1) (Settings.combine t thist) s ()
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
                    (0.0, chosen))
          in
          Option.value ~default:[] r




let rec line_right () =
     let c = choose 
          [ 
               0.1, shape line_right + (sc 0.999 0.999) + (tr (0.5) 0.)
               + (rot 4.);
               0.1, shape line_right +  (sc 0.999 0.999) + (tr (0.5) 0.)  + (rot (-.4.));
          ]
     in
     square + (v 9) + (sc 1. 0.4)
     ++ shape c

let rec line_down () =
     many 20 (tr 0. 3.) ((shape line_right) + (v 9))

let f = line_down
