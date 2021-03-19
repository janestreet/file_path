open! Core
open! Async

(** We test [File_path_io.S] implementations via functor. Because of restrictions on
    expect tests in functors, we instantiate the tests here rather than in client
    libraries. *)

module Test_file_path_async : File_path_io.S with type 'a io := 'a Deferred.t
module Test_file_path_core : File_path_io.S with type 'a io := 'a
