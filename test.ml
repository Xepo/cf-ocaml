open Core.Std
open Vec.Infix
open Matrix.Infix

let _ =
     assert (Vec.in_unit (0.4,0.4));
     assert (not (Vec.in_unit (50.4,0.4)));
     assert (not (Vec.in_unit (0.4,50.4)))



let assert_eq eq to_str x y = 
     if eq x y 
     then ()
     else
          printf "Assert failure: %s vs. %s\n" (to_str x) (to_str y)


let _ = 
     let v:Vec.t = (5.0,5.0) in
     let m = Matrix.identity in
     let r:Vec.t = m *|$ v in
     assert (r =$ v);
     assert ((m *| m) =| m);
     let assert_eqv = assert_eq (=) (Matrix.Row.to_string) in
     let assert_eqm = assert_eq (=|) (Matrix.to_string) in

     let m1 = (Matrix.translate 5. 1.) *| (Matrix.translate 10. 2.) in
     let m2 = (Matrix.translate 15. 3.) in
     assert_eqm m1 m2;
     assert ((m1 *|$ v) =$ (m2 *|$ v));


     let v:Vec.t = (5.0,-1.0) in
     let m1 = Matrix.translate 5. 1. in
     assert ((m1 *|$ v) =$ (10.0,0.0));

     let v:Vec.t = (5.0,-1.0) in
     let m1 = Matrix.scale 2. 3. in
     assert ((m1 *|$ v) =$ (10.0,-3.0));

     assert ((Matrix.invert Matrix.identity) =| Matrix.identity);
     assert ((Matrix.invert (Matrix.invert m1)) =| m1);
     assert ((Matrix.invert (Matrix.invert (m1*|m2))) =| (m1*|m2));
     let m1 = Matrix.scale 2. 4. in
     let m2 = Matrix.scale 0.5 0.25 in
     assert ((Matrix.invert m1) =| m2);

     let sc = Matrix.scale 2. 4. in
     let tr = Matrix.translate 10. 5. in
     let m = sc *| tr in
     Vec.assert_equal 
     ((m *|$ (0.,0.))) (20.,20.);

     let v = (30.,-30.) in
     let m = Matrix.translate 10. 5. in
     let m2 = Matrix.invert m in
     let v2 = m2 *|$ v in
     Vec.assert_equal v2 (20.,-35.)



let _ = 
     let output = Outputtable.create ~pixelwidth:20 ~pixelheight:20
     ~pixelborder:0 () in
     let output = Outputtable.new_viewport output (((-5.),(-5.)),(5.,5.)) in
     let (x1,y1) = (Outputtable.pixel_to_space output (0,0)) in
     let (x2,y2) = (Outputtable.pixel_to_space output
     (output.Outputtable.pixelwidth,output.Outputtable.pixelheight)) in
     (printf "pixelviewport:%f,%f %f,%f\n" x1 y1 x2 y2)
     ;;



let tra ?(x=0.0) ?(y=0.0) ?(w=1.0) ?(h=1.0) ?(rot=0.0) () =
     (Matrix.translate x y
     *| Matrix.rotate rot
     *| Matrix.scale w h)

let _ = 
     let n = Rect.apply Rect.unit_rect (tra ~x:5.0 ~rot:90. ~w:2.0 ~h:2.0 ())
     in
     printf "%s\n\n" (Rect.to_string n)

     
