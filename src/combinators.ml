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
end

module Make (In : Input) : S 
with type input = In.t and type elt = In.elt = struct
    type error = string
    type input = In.t
    type elt = In.elt
    type 'a t = { run: input -> input * ('a, error) result }

    exception ParseExn of string

    let return x = { run = (fun inp -> inp, Ok x) }
    let fail err = { run = (fun inp -> inp, Error err) }
    let apply p inp = p.run inp

    let bind p f =
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> (f x).run inp'
                | _, Error _ as e -> e }

    let map p f =
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> inp', Ok (f x)
                | _, Error _ as e -> e }

    let forward p q =
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> let _, x' = q.run x in inp', x'
                | _, Error _ as e -> e }

    let left p q = 
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> (match q.run inp' with
                    | inp'', Ok _ -> inp'', Ok x
                    | _, Error e -> inp, Error e)
                | _, Error _ as e -> e }

    let right p q = 
        { run = fun inp ->
            match p.run inp with
                | inp', Ok _ -> (match q.run inp' with
                    | _, Ok _ as x -> x
                    | _, Error e -> inp, Error e)
                | _, Error _ as e -> e }

    let both p q = 
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> (match q.run inp' with
                    | inp'', Ok y -> inp'', Ok (x, y)
                    | _, Error e -> inp, Error e)
                | _, Error _ as e -> e }

    let any p q = 
        { run = fun inp ->
            match p.run inp with
                | inp', Error _ -> q.run inp'
                | _, Ok _ as res -> res }

    let opt p d = return d |> any p 

    let map2 ~f p q =
        { run = fun inp ->
            match (both p q).run inp with
                | inp', Ok (x, y) -> inp', Ok (f x y)
                | _, Error _ as e -> e }

    let map3 ~f p q r =
        { run = fun inp ->
            match (both p (both q r)).run inp with
                | inp', Ok (x, (y, z)) -> inp', Ok (f x y z)
                | _, Error _ as e -> e }

    let repeat0 p = 
        { run = fun inp ->
            let buf = ref [] in
            let rec inner inp = match p.run inp with
                | inp', Ok x -> buf := x :: !buf; inner inp'
                | inp', Error _ -> (inp', Ok (List.rev !buf))
            in inner inp }

    let repeat1 p =
        { run = fun inp ->
            match p.run inp with
                | inp', Ok x -> 
                    let inp'', res = (repeat0 p).run inp'
                        in (inp'', Ok (x :: Result.get_ok res))
                | _, Error _ as e -> e }

    let foreach p q =
        { run = fun inp ->
            let rec elt = function
                | [] -> []
                | x :: xs -> match q.run x with
                    | _, Ok x' -> x' :: (elt xs)
                    | _, Error e -> raise (ParseExn e)
            in match p.run inp with
                | inp', Ok xs -> (try inp', Ok (elt xs)
                    with ParseExn e -> inp, Error e)
                | _, Error e -> inp, Error e }

    let elt_if f =
        { run = fun inp ->
            try let c = In.get inp 0 in
                if f c
                then (return c).run (In.drop 1 inp)
                else (fail "").run inp
            with Invalid_argument _ -> (fail "").run inp }

    let elt c = elt_if (fun x -> x = c)

    let exact s =
        { run = fun inp ->
            match In.drop_match s inp with
                | Some s' -> (return s).run s' 
                | None -> (fail "").run inp }

    let take_all = { run = fun inp -> In.empty, Ok inp }
    let take_while f = map (elt_if f |> repeat1) In.join
    let take_until f = take_while (fun x -> not (f x))

    module O = struct
        let ( >>= ) = bind
        let ( >>| ) = apply
        let ( >>$ ) = map
        let ( >>* ) = forward
        let ( <*  ) = left
        let (  *> ) = right
        let ( <*> ) = both
        let ( <|> ) = any
        let ( <?> ) = opt
    end
end

module String = String
