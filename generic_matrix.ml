open Core.Std
let pi = 3.14159

                    (*
let permutations l =
     let insert_at_every l v =
          List.fold (List.range 0 (1+(List.length l))) ~init:[] ~f:(fun acc i ->
               let (l1,l2) = List.split_n l i in
               (l1 @ [v] @ l2) :: acc)
     in
     let rec r l = 
          match l with
          | [] | [_] -> [l]
          | x :: rest ->
               let perms = r rest in
               List.fold perms ~init:[] ~f:(fun acc l ->
                    (insert_at_every l x) @ acc
               )
     in
     r l

               let rec list_combine l1 l2 = 
                    match l1,l2 with
                    | [],[] -> []
                    | x :: l1, y :: l2 ->
                              (x,y) :: (list_combine l1 l2)
                    | _ -> raise (Invalid_argument "Lists must have same length")



               let determinant t = 
                    let range = List.range 0 (List.length Row.indices) in
                    let perms = permutations (list_combine range Row.indices) in
                    let mul t l = 
                         let (ret, _) = 
                              List.fold l ~init:(1., Row.indices)
                              ~f:(fun (acc, indices) j -> 
                                   match indices with
                                   | [] -> (acc,[])
                                   | i :: indices -> 
                                        let v = get i j t in
                                        ((v *. acc), indices)
                              )
                         in
                         ret
                    in
                    let rec pos_or_neg = function
                         | [] | [_] -> 1.
                         | x :: y :: rest ->
                              let this = 
                                   if y < x then -1. else 1.
                              in
                              this *. (pos_or_neg rest)
                    in
                    let perms = List.map perms ~f:(fun l ->
                         (pos_or_neg l,
                         List.map l ~f:(fun (_,x) -> x)))
                    in
                    fun t ->
                         List.fold perms ~init:0. ~f:(fun acc (neg,perm) ->
                              acc +. (neg *. (mul t perm)))

                         *)
module type ROW = sig
     type index 
     val indices : index list

     type 'a t
     val create : 'a -> 'a t
     val get : index -> 'a t -> 'a
     val set : index -> 'a -> 'a t -> 'a t
end;;



module Matrix = 
     functor (Row: ROW) ->
          struct
               module Row_g = struct
                    include Row
                    let dot v1 v2 =
                         List.fold indices ~init:(0.) ~f:(fun acc i ->
                              ((get i v1) *. (get i v2)) +. acc)
                    let (=) v1 v2 =
                         List.fold indices ~init:true
                         ~f:(fun acc i -> acc && ((get i v1) =. (get i v2)))
                    let to_string row = 
                         "[" ^
                         (String.concat ~sep:"," (List.map Row.indices 
                              ~f:(fun i -> sprintf "%f" (Row.get i row))))
                         ^ "]"
               end
               type t = float Row_g.t Row_g.t

               let get i j t = 
                    Row_g.get j (Row_g.get i t)

               let set i j v t = 
                    let row = Row_g.set j v (Row_g.get i t) in
                    Row_g.set i row t

               let indices = 
                    List.fold Row_g.indices ~init:[] ~f:(fun acc i -> 
                         List.fold Row_g.indices ~init:acc ~f:(fun acc j ->
                              (i,j) :: acc))

               let row = Row_g.get
               let col t j = 
                    List.fold 
                         Row_g.indices 
                         ~init:(Row_g.create 0.)
                         ~f:(fun acc i ->
                              Row_g.set i (get i j t) acc)

               let initialize f = 
                    List.fold indices
                         ~init:(Row_g.create (Row_g.create 0.0))
                         ~f:(fun acc (i,j) -> set i j (f (i,j)) acc)

               let identity =
                    initialize (fun (i,j) ->
                         if i = j then 1.0 else 0.0)

               let mapi t ~f =
                    initialize (fun (i,j) ->
                         f (i,j) (get i j t) )

               let map ~f = mapi ~f:(fun (_,_) v -> f v)

               let multiply t1 t2 =
                    let do_cell (i,j) = 
                         List.fold Row_g.indices ~init:0.0 ~f:(fun acc k ->
                              acc +. (get i k t1) *. (get k j t2))
                    in
                    initialize do_cell

               let mul_vec t vec = 
                    List.fold indices ~init:(Row_g.create 0.)
                         ~f:(fun acc (i,j) ->
                              let v = Row_g.get i acc in
                              let v = v +. (get i j t) *. (Row_g.get j vec) in
                              Row_g.set i v acc)

               let transpose t = 
                    initialize (fun (i,j) -> get j i t)

               let to_string t =
                    String.concat ~sep:"\n" 
                    (List.map Row_g.indices 
                    ~f:(fun i -> Row_g.to_string (Row_g.get i t)))

               let translate : float Row_g.t -> t  =
                    let last_index = List.last_exn Row_g.indices in
                    fun vec ->
               initialize (fun (i,j) ->
                    if j = last_index then Row_g.get i vec
                    else get i j identity)

               let scale vec : t =
                    initialize (fun (i,j) ->
                         if i = j then Row_g.get i vec
                         else 0.0
                    )

               let rotate i j d : t = 
                    let d = (d *. pi /. 180.) in
                    let ret = identity in
                    let ret =  set i i (cos d) ret in
                    let ret =  set j j (cos d) ret in
                    let ret =  set i j (-. sin d) ret in
                    let ret =  set j i (sin d) ret in
                    ret

               let eq t1 t2 =
                    List.fold Row_g.indices 
                    ~init:true
                    ~f:(fun acc i -> acc && (Row_g.(=) (row i t1) (row i t2)))

               module Infix_g = struct
                    let (=|) = eq
                    let ( *|) = multiply
               end

          end;;


module T3 = struct
     type index =  [`i1 | `i2 | `i3]
     let indices = [`i1 ; `i2 ; `i3;]
     let index_eq x y =
          match x,y with
          | `i1,`i1 
          | `i2,`i2
          | `i3,`i3 -> true
          | _ -> false


     type 'a t = ('a * 'a * 'a)
     let create v = (v,v,v)
     let get (index:index) ((c1,c2,c3):'a t) : 'a = 
          match index with
          | `i1 -> c1
          | `i2 -> c2
          | `i3 -> c3

     let set index v (c1,c2,c3) = 
          match index with
          | `i1 -> (v,c2,c3)
          | `i2 -> (c1,v,c3)
          | `i3 -> (c1,c2,v)

end


module T4 = struct
     type index =  [`i1 | `i2 | `i3 | `i4]
     let indices = [`i1 ; `i2 ; `i3 ; `i4;]

     type 'a t = ('a * 'a * 'a * 'a)
     let create v = (v,v,v,v)
     let get (index:index) ((c1,c2,c3,c4):'a t) : 'a = 
          match index with
          | `i1 -> c1
          | `i2 -> c2
          | `i3 -> c3
          | `i4 -> c4

     let set index v (c1,c2,c3,c4) = 
          match index with
          | `i1 -> (v,c2,c3,c4)
          | `i2 -> (c1,v,c3,c4)
          | `i3 -> (c1,c2,v,c4)
          | `i4 -> (c1,c2,c3,v)

end
module Matrix3 = Matrix(T3);;

let assert_eq eq to_str x y = 
     if eq x y 
     then ()
     else
          printf "Assert failure: %s vs. %s\n" (to_str x) (to_str y)

open Vec.Infix
let _ = 
     let open Matrix3.Infix_g in
     let v = (5.0,5.0,1.0) in
     let m = Matrix3.identity in
     let r = Matrix3.mul_vec m v in
     let assert_eqv = assert_eq (=) (Matrix3.Row_g.to_string) in
     let assert_eqm = assert_eq (=|) (Matrix3.to_string) in
     assert_eqv r v;
     assert_eqm m (m *| m);
     let trans = Matrix3.translate (1.,0.,1.) in
     assert_eqm trans (Matrix3.set `i1 `i3 1.0 Matrix3.identity);
     assert_eqm (trans *| trans) (Matrix3.set `i1 `i3 2.0 Matrix3.identity);

     (*
     let m1 = (Matrix.translate 5. 1.) *| (Matrix.translate 10. 2.) in
     let m2 = (Matrix.translate 15. 3.) in
     assert (m1 =| m2);
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

*)
