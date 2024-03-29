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
     square *- (red) *- (sc 1. 0.4)
     ++ shape c

let rec line_down () =
     many (360 / 100) ((tr 0. 4.) +| (rot 3.)) (shape line_right)

let rec line () = 
     square *- (red) *- (sc 1. 0.4)
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
     many (360 / 45) (rot 45.) (circle *- (tr 2. 0.) *- (red))

let rec t1 () = 
     shape surround ++
     many 4 (rot 90.) (shape t1 *- (tra ~x:2.0 ~s:0.4 ()))


(* t2 *)
let square = square *- (c 1.0 1.0 1.0)
let rec sq () =
     begin 
     (choose
     [
          0.9, (square ++ 
               (square *- (tra ~w:1.2 ~h:0.65 ())));

          0.5, (shape (r (bri 0.96)) *- (sc 0.5 0.5)) ++
               (shape (r (bri 0.96)) *- (flip_y) *- (sc 0.5 0.5)) ++
               square ++
               (square *- (tra ~s:0.7 ()));
     ] ()) 
     end

and r c () =
     many 160 (c +| (tra ~x:1.0 ~rot:1.0 ~s:0.98 ())) (shape sq)


let m () =
     (circle *- (sc 38. 38.)) ++ (*brightness*)
     (circle *- (sc 39. 39.) *- (white)) ++
     many 8 (rot 45.)
          (shape (r ((bri 0.96) +| (crot_rb 10.0))) *- (tra ~x:20. ~s:2. ()))

let t2 () =
     many 5 ((tra ~s:0.5 ~rot:11. ()))
     (shape m *- (c 0.01 0.0 0.0))

     (*TODO: pixelborder screws up t1*)
     (*TODO: *- v 9 on surround isn't working*)
let _ = Scene.render_scene ~w:700 ~h:700 ~alias:3 ~bg:(0.0,0.0,0.0,0.0) ~filename:"image.png" t2
