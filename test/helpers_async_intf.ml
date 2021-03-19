open! Core
open! Async

module type Path = sig
  type t

  val of_string : string -> t
  val arg_type : t Command.Arg_type.t
end

module type Helpers_async = sig
  (** Tests autocompletion of file paths using the given arg type. *)
  val test_arg_type
    :  (module Path with type t = 'a)
    -> expect_output:(unit -> string)
    -> unit Deferred.t
end
