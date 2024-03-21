module type Input = sig
    type t
    type elt

    val empty: t
    val get : t -> int -> elt
    val take : int -> t -> t
    val drop : int -> t -> t
    val drop_match : t -> t -> t option
    val join : elt list -> t
end

module type S = sig
    type error = string
    type input
    type elt
    type 'a t

    exception ParseExn of string

    val return : 'a -> 'a t
    val fail : error -> 'a t

    val apply : 'a t -> input -> input * ('a, error) result
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val map : 'a t -> ('a -> 'b) -> 'b t
    val forward : input t -> 'a t -> 'a t
    val left : 'a t -> 'b t -> 'a t
    val right : 'a t -> 'b t -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val any : 'a t -> 'a t -> 'a t
    val opt : 'a t -> 'a -> 'a t

    val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val map3 : f:('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
    val repeat0 : 'a t -> 'a list t
    val repeat1 : 'a t -> 'a list t
    val foreach : input list t -> 'a t -> 'a list t

    val elt_if : (elt -> bool) -> elt t
    val elt : elt -> elt t
    val exact : input -> input t
    val take_all : input t
    val take_while : (elt -> bool) -> input t
    val take_until : (elt -> bool) -> input t

    module O : sig
        val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
        val ( >>$ ) : 'a t -> ('a -> 'b) -> 'b t
        val ( >>| ) : 'a t -> input -> input * ('a, error) result
        val ( >>* ) : input t -> 'a t -> 'a t
        val ( <*  ) : 'a t -> 'b t -> 'a t
        val (  *> ) : 'a t -> 'b t -> 'b t
        val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
        val ( <|> ) : 'a t -> 'a t -> 'a t
        val ( <?> ) : 'a t -> 'a -> 'a t
    end
    
    include module type of O
end

module Make (In : Input) : S with type input = In.t and type elt = In.elt
