module DLS = struct
  type 'a key =
    { init : unit -> 'a
    ; cell : 'a option ref }

  let new_key ?split_from_parent:_ fn =
    { init= fn; cell= ref None }

  let get { init; cell; } = match !cell with
    | None -> cell := Some (init ()); Option.get !cell
    | Some value -> value

  let set { cell; _ } value = cell := Some value
end
