open Scene

let rec line_right () =
     let c = choose 
          [ 
               0.05, shape line_right *- (rot 4.) *- (sc 0.999 0.999) *- (tr (0.5) 0.)  ++
                    shape line_right *- (rot (-. 4.)) *- (sc 0.999 0.999) *- (tr (0.5) 0.) 
               ;
              0.05, shape line_right *- (flip_y);
               10.0, shape line_right *-  (sc 0.999 0.999) *- (tr (0.5) 0.) *- (rot 0.5)  ;
          ]
     in
     square *- (v 9) *- (sc 1. 0.4)
     ++ shape c

let rec line_down () =
     many (360 / 100) ((tr 0. 4.) +| (rot 3.)) (shape line_right)

let rec line () = 
     square *- (v 9) *- (sc 1. 0.4)
     ++ shape line *- (tr 0.5 0.)
let scene () =
     shape line_down *- (rot 0.) ++
     shape line_down *- (rot 90.) ++
     shape line_down *- (rot 60.) ++
     shape line_down *- (rot 80.)

let rot_line () =
     (shape line *-  (tr 0. 5.) *- (rot 45.)) ++
     (shape line *- (rot 45.) *- (tr 0. 5.)) ++
     shape line 


(* t1 *)
let surround () = 
     many (360 / 45) (rot 45.) (circle *- (tr 2. 0.) *- (v 0))

let rec t1 () = 
     shape surround ++
     many 4 (rot 90.) (shape t1 *- (tra ~x:2.0 ~s:0.4 ()))





     (*TODO: *- v 9 on surround isn't working*)
let _ = Scene.render_scene ~w:700 ~h:700 ~alias:3 ~bg:9 t1
