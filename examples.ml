let f x = (* implementation *) in
(f 1, f 1)

(* SHOULD be equal to this, right? *)

let f x = (* implementation *) in
let r = f 1 in
(r, r)




(* 'a option        - result not available
 * 'a list          - many results / non-determinism
 * int -> 'a        - interger read-only state
 * int -> ('a, int) - integer state
 * ('a, string)     - output
 *)





module Option_idiom = struct
  type 'a t = None | Some of 'a

  let map f = function
    | None   -> None
    | Some x -> Some (f x)

  let pure x = Some x
  let apply mf mx = match mf, mx with
    | Some f, Some x -> Some (f x)
    | _              -> None

  let (#) = apply
end

(* Examples *)

val a, b, c : int Option_idiom.t
val x, y, z : int
val mult : int -> int -> int
val div : int -> int -> int Option_idiom.t

mult x y (* OK! *)
mult a b (* WRONG! *)
pure mult (#) a (#) b (* OK *)
pure (fun a b c -> a * b + c) (#) a (#) b (#) c





module Option_arrow = struct
  type ('a, 'b) t = 'a -> 'b option

  let arr f = fun x -> Some (f x)

  let (>>>) f g = fun x -> match f x with
    | None   -> None
    | Some y -> g y

  let first f = fun (x, c) ->
    match f x with
      | None   -> None
      | Some y -> (y, c)
end

(* EXAMPLES *)

val div : (int * int, int) Option_arrow.t

first div >>> div :: ((int * int) * int, int) Option_arrow.t







module Option_monad = struct
  type 'a t = None | Some of 'a

  let return x = Some x
  let bind m f = match m with
    | None   -> None
    | Some x -> f x
end



module State = struct
  type 'a t = int -> ('a, int)

  let return x = fun s -> (x, s)
  let bind m f = fun s ->
    let (x, s')  = m s in
    f x s'

  let (>>=) = bind

  let pure a = fun s -> (a, s)
  let apply f a = fun s ->
    let (f, s')  = f s in
    let (a, s'') = a s' in
    (f a, s'')

  type ('a, 'b) arr = 'a -> 'b t
  (* ARROW INTERFACE *)

  let get () = fun s -> (s, s)
  let put t = fun _ -> ((), s)
end

let freshName =
  get ()
  >>= fun s ->
  put (s+1)
  >>= fun () ->
  return (sprintf "%d" s)

let ifZero m1 m2 =
  get ()
  >>= fun s ->
  if s = 0 then m1 else m2

let getTransformed f =
  get ()
  >>= fun s ->
  return (f s)


let freshName =
  get >>> arr (fun x -> (x+1, x)) >>> first put >>> arr snd




let getTransformed f =
  pure f (#) get
