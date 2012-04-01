open Core.Std

let (=$) = Vec.(=$)
let ( *|$ ) = Matrix.( *|$ )
let ( *| ) = Matrix.( *|)
let ( =| ) = Matrix.( =|)

let repeat ~f x n =
     let rec ret = ref x in
     for i = 0 to (n-1)
     do
          ret := f (!ret)
     done;
     !ret
     
let split_fcns_basics =
     List.fold ~init:([],[]) ~f:(fun (basics, fcns) s ->
          match s.Renderable.shape with
          | Renderable.Shapes.Basic _ -> ((s :: basics), fcns)
          | Renderable.Shapes.Fcn _ -> (basics, (s :: fcns)))

let expand_list (basics, fcns) =
     printf "expanding: %d %d\n" (List.length basics) (List.length fcns);
     let (b,f) = 
          split_fcns_basics 
               (List.concat (List.map fcns ~f:Renderable.expand) )
     in
     (b @ basics, f)

let rec expand_until (basics, fcns) = 
     printf "expanding: %d %d\n" (List.length basics) (List.length fcns);
     let fcns = List.filter fcns 
          ~f:(fun x -> 0.001 <= Context.size x.Renderable.context) 
     in
     let (b,f) = 
          split_fcns_basics 
               (List.concat (List.map fcns ~f:Renderable.expand) )
     in
     let ret = b @ basics, f in
     match f with
     | [] -> ret
     | _ -> expand_until ret
     


let main () =
     printf "init\n";
     Random.self_init ();
     let output = Outputtable.create ~pixelwidth:(700) ~pixelheight:(700) ~alias:5 ()
     in
     let shapes = Scene.f () in
     (*let (basics,_) = repeat ~f:expand_until ([], shapes) 200 in*)
     let (basics,_) = expand_until ([], shapes) in
     let shapes = basics in
     let rc = List.fold shapes ~init:None 
          ~f:(fun rc shape -> 
               if Renderable.is_basic shape 
               then 
                    let mat = shape.Renderable.context in
                    match rc with
                    | None -> Some (Rect.expand_unit mat)
                    | Some rc -> Some (Rect.expand rc (Rect.expand_unit mat))
               else
                    rc)
     in
     let rc = Option.value ~default:Rect.unit_rect rc in
     let output = Outputtable.new_viewport output rc in
     List.iter shapes ~f:(fun r ->
          printf "%s\n" (Renderable.to_string r));
     let () =
          List.iter shapes 
          ~f:(fun t -> Renderable.render output t)
     in
     printf "%s\n" (Outputtable.string_of_viewport output);
     let aliased = Outputtable.antialias output in
     Outputtable.write_arr aliased "image.in";
     printf "Done\n"
     (*printf "%s\n" (Outputtable.to_string output)*)

let _ = main ()

