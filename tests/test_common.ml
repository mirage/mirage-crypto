open OUnit2

let (prf, strf) = Format.(fprintf, asprintf)
let pp_map pp f ppf x = pp ppf (f x)
let pp_diff pp ppf (a, b) = prf ppf "@[<v>want: %a@,have: %a@]" pp a pp b

let of_hex ?(skip_ws = true) s =
  let fold f acc str =
    let st = ref acc in
    String.iter (fun c -> st := f !st c) str;
    !st
  and digit c =
    match c with
    | '0'..'9' -> int_of_char c - 0x30
    | 'A'..'F' -> int_of_char c - 0x41 + 10
    | 'a'..'f' -> int_of_char c - 0x61 + 10
    | _ -> invalid_arg "bad character"
  and is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false
  in
  let chars, leftover =
    fold (fun (chars, leftover) c ->
        if skip_ws && is_space c then
          chars, leftover
        else
          let c = digit c in
          match leftover with
          | None -> chars, Some (c lsl 4)
          | Some c' -> (c' lor c) :: chars, None)
      ([], None) s
  in
  let chars = List.rev chars in
  assert (leftover = None);
  String.init (List.length chars) (fun i -> char_of_int (List.nth chars i))

let rec range a b =
  if a > b then [] else a :: range (succ a) b

let rec times ~n f a =
  if n > 0 then ( ignore (f a) ; times ~n:(pred n) f a )

let pp_opt pp ppf = Format.(function
  | Some x -> fprintf ppf "Some(%a)" pp x
  | None   -> fprintf ppf "None")

let eq_opt eq a b = match (a, b) with
  | (Some x, Some y) -> eq x y
  | _                -> false

let pp_octets pp = pp (Ohex.pp_hexdump ())

let assert_oct_equal ?msg =
  assert_equal ~cmp:String.equal ?msg ~pp_diff:(pp_octets pp_diff)

let iter_list xs f = List.iter f xs

let cases_of f =
  List.map @@ fun params -> test_case (f params)

let any _ = true

let vx = Ohex.decode

let f1_eq ?msg f (a, b) _ =
  assert_oct_equal ?msg (f (vx a)) (vx b)

let f2_eq ?msg f (a, b, c) = f1_eq ?msg (f (vx a)) (b, c)
