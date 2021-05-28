open! Core

module type S = sig
  type t

  val arg_type : t Command.Arg_type.t
end

module Quoted = struct
  type t = unit

  let arg_type =
    Command.Arg_type.create ignore ~complete:(fun (_ : Univ_map.t) ~part ->
      [ ""; part; sprintf "%S" part ])
  ;;
end

let subcommand name (module M : S) =
  let command =
    Command.basic
      ~summary:(sprintf "%s completion" name)
      (let%map_open.Command (_ : M.t) = anon (name %: M.arg_type) in
       fun () -> ())
  in
  name, command
;;

let () =
  Command.group
    ~summary:"tab-completion examples"
    [ subcommand "path" (module File_path)
    ; subcommand "absolute" (module File_path.Absolute)
    ; subcommand "relative" (module File_path.Relative)
    ; subcommand "part" (module File_path.Part)
    ; subcommand
        "filename"
        (module struct
          type t = Filename.t

          let arg_type = Filename_unix.arg_type
        end)
    ; subcommand "mode" (module Prod_or_dev)
    ; subcommand "quote" (module Quoted)
    ]
  |> Command_unix.run
;;
