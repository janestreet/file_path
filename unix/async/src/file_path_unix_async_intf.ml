(** Asynchronous I/O operations for [File_path] types. See [../../doc/file-path.mdx]. *)

open! Core
open! Async

module type S = sig
  include File_path_io.S with type 'a io := 'a Deferred.t (** @open *)
end

module type File_path_unix_async = sig
  module type S = S

  include S (** @open *)
end
