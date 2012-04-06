open Core.Std
module Settings : sig
     type t
end

val (=$) : Vec.t -> Vec.t -> bool
val ( *|$ ) : Matrix.t -> Vec.t -> Vec.t
val ( *| ) : Matrix.t -> Matrix.t -> Matrix.t
val (=|) : Matrix.t -> Matrix.t -> bool
val (+|) : Settings.t -> Settings.t -> Settings.t

val flip_y : Settings.t
val circle : Renderable.t list
val square : Renderable.t list

val shape : ?trace:string -> (unit -> Renderable.t list) -> Renderable.t list

val add_trace : string -> Renderable.t list -> Renderable.t list

val ( *- ) : Renderable.t list -> Settings.t -> Renderable.t list
val ( ++ ) : Renderable.t list -> Renderable.t list -> Renderable.t list

val tr : float -> float -> Settings.t
val tra : ?x:float -> ?y:float -> ?w:float -> ?h:float -> ?rot:float -> unit ->
     Settings.t
val sc : float -> float -> Settings.t
val rot : float -> Settings.t
val v : int -> Settings.t

val many : int -> Settings.t -> Renderable.t list -> Renderable.t list 
val choose : (float * Renderable.t list) list -> unit -> Renderable.t list
val render_scene :
     w:int -> h:int -> ?alias:int -> (unit -> Renderable.t list) -> unit
