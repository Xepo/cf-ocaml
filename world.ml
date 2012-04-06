open Core.Std
open Vec.Infix
open Matrix.Infix

type t = 
     {
          basics: Renderable.t list;
          fcns: Renderable.t list Float.Map.t;
          basic_count : int;
          extents : Rect.t option;
     }

let empty = {basics=[]; fcns=Float.Map.empty; basic_count=0; extents=None}

let add_to_map map x = 
     let size = Settings.size_of_unit x.Renderable.settings in
     Float.Map.change map size
     (function
          | None -> Some [x]
          | Some l -> Some (x :: l))

let rec pop_from_map map =
     match Float.Map.max_elt map with
     | None -> (map, None)
     | Some (key, []) -> 
               pop_from_map (Float.Map.remove map key)
     | Some (key,ret :: data) ->
          (Float.Map.add map ~key ~data,
               Some ret)

let largest_size t =
     match Float.Map.max_elt t.fcns with
     | None -> -.1.0
     | Some (key,_) -> key

let add_basic t x =
     if 0 = ((t.basic_count+1) mod 1000)
     then 
          printf 
               "Expanded %d shapes, largest size %f\n" 
               (t.basic_count+1)
               (largest_size t);
     let xextent = Rect.containing_rect_of_unit x.Renderable.settings.Settings.transform in
     let extents = 
          match t.extents with
          | None -> Some xextent
          | Some extents -> Some (Rect.expand extents xextent)
     in
     {t with basics=x :: t.basics; basic_count=t.basic_count+1; extents;}

let add t x =
     if Renderable.is_basic x 
     then add_basic t x
     else {t with fcns=add_to_map t.fcns x}

let add_list t l =
     List.fold l ~init:t ~f:(fun acc x ->
          add acc x)

let pop_fcn t = 
     let fcns, ret = 
          pop_from_map t.fcns 
     in
     ({t with fcns}, ret)

let calculate_threshold ~pixelwidth ~pixelheight world = 
     let extents = Option.value_exn world.extents in
     min 
          ((Rect.width extents) /. (float_of_int pixelwidth))
          ((Rect.height extents) /. (float_of_int pixelheight))

let rec expand_until_size ~pixelwidth ~pixelheight world =
     let rec recurs ~size_threshold rec_amt world =
          if rec_amt <= 0
          then 
               (printf "Hit rec limit\n";
               world)
          else
               let (world, this) = pop_fcn world in
               match this with
               | None -> world
               | Some this ->
                    let size_threshold =
                         if world.basic_count > 0
                         then calculate_threshold ~pixelwidth ~pixelheight world 
                         else size_threshold
                    in
                    if world.basic_count > 0 &&
                         Settings.size_of_unit this.Renderable.settings <. size_threshold
                    then add world this
                    else
                         let expanded = Renderable.expand this in
                         let world = 
                              List.fold expanded ~init:world ~f:add
                         in
                         recurs ~size_threshold (rec_amt - 1) world
     in
     recurs ~size_threshold:100000.0 20000 world
