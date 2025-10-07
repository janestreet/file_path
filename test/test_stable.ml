open! Core

module Part = struct
  module V1 = struct
    type t = File_path.Stable.Part.V1.t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]

    type comparator_witness = File_path.Stable.Part.V1.comparator_witness

    let comparator = File_path.Stable.Part.V1.comparator
    let hashable = File_path.Stable.Part.V1.hashable

    let%expect_test _ =
      Helpers.test_stable_version
        [%here]
        (module File_path.Stable.Part.V1)
        Examples.Part.for_conversion;
      [%expect
        {|
        (bin_shape_digest 2c658a7c91fc0a7cf9af9aae6b799d09)
        ((sexp .) (bin_io "\001."))
        ((sexp ..) (bin_io "\002.."))
        ((sexp filename.txt) (bin_io "\012filename.txt"))
        ((sexp bin) (bin_io "\003bin"))
        ((sexp .hidden) (bin_io "\007.hidden"))
        ((sexp "This is a sentence; it has punctuation, capitalization, and spaces!")
         (bin_io
          "CThis is a sentence; it has punctuation, capitalization, and spaces!"))
        ((sexp "\001\255") (bin_io "\002\001\255"))
        |}]
    ;;

    module Map = File_path.Stable.Part.V1.Map
    module Set = File_path.Stable.Part.V1.Set
    module Table = File_path.Stable.Part.V1.Table
    module Hash_set = File_path.Stable.Part.V1.Hash_set

    let%expect_test _ =
      Helpers.test_stable_containers
        [%here]
        (module File_path.Stable.Part.V1)
        Examples.Part.for_conversion;
      [%expect
        {|
        ((name Map) (bin_shape_digest 727af34db3b3d8fe9ec75bd4bde3e79f))
        ((name Set) (bin_shape_digest c3978702a055dec09b2070750427b070))
        ((name Table) (bin_shape_digest cd88244b145d18bce45aa35a507adebe))
        ((name Hash_set) (bin_shape_digest 08de2ea8fcec183ba72fc76acd65937f))
        (Set
         ("\001\255"
          .
          ..
          .hidden
          "This is a sentence; it has punctuation, capitalization, and spaces!"
          bin
          filename.txt))
        (Map
         (("\001\255" 6)
          (. 0)
          (.. 1)
          (.hidden 4)
          ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
          (bin 3)
          (filename.txt 2)))
        (Hash_set
         ("\001\255"
          .
          ..
          .hidden
          "This is a sentence; it has punctuation, capitalization, and spaces!"
          bin
          filename.txt))
        (Table
         (("\001\255" 6)
          (. 0)
          (.. 1)
          (.hidden 4)
          ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
          (bin 3)
          (filename.txt 2)))
        |}]
    ;;
  end
end

module Relative = struct
  module V1 = struct
    type t = File_path.Stable.Relative.V1.t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]

    type comparator_witness = File_path.Stable.Relative.V1.comparator_witness

    let comparator = File_path.Stable.Relative.V1.comparator
    let hashable = File_path.Stable.Relative.V1.hashable

    let%expect_test _ =
      Helpers.test_stable_version
        [%here]
        (module File_path.Stable.Relative.V1)
        Examples.Relative.for_conversion;
      [%expect
        {|
        (bin_shape_digest 7c63400e8c30043c29264293375a91dc)
        ((sexp .) (bin_io "\001."))
        ((sexp ..) (bin_io "\002.."))
        ((sexp filename.txt) (bin_io "\012filename.txt"))
        ((sexp bin) (bin_io "\003bin"))
        ((sexp .hidden) (bin_io "\007.hidden"))
        ((sexp "This is a sentence; it has punctuation, capitalization, and spaces!")
         (bin_io
          "CThis is a sentence; it has punctuation, capitalization, and spaces!"))
        ((sexp "\001\255") (bin_io "\002\001\255"))
        ((sexp ./.) (bin_io "\003./."))
        ((sexp ../..) (bin_io "\005../.."))
        ((sexp ././.) (bin_io "\005././."))
        ((sexp bin/exe) (bin_io "\007bin/exe"))
        ((sexp bin/exe/file) (bin_io "\012bin/exe/file"))
        |}]
    ;;

    module Map = File_path.Stable.Relative.V1.Map
    module Set = File_path.Stable.Relative.V1.Set
    module Table = File_path.Stable.Relative.V1.Table
    module Hash_set = File_path.Stable.Relative.V1.Hash_set

    let%expect_test _ =
      Helpers.test_stable_containers
        [%here]
        (module File_path.Stable.Relative.V1)
        Examples.Relative.for_conversion;
      [%expect
        {|
        ((name Map) (bin_shape_digest 0a9d2f751b6585546ea48d815e859877))
        ((name Set) (bin_shape_digest 62c6b347cb5af74a577a7c63c152df38))
        ((name Table) (bin_shape_digest 5342018d47459f582dcecacfc3e6633e))
        ((name Hash_set) (bin_shape_digest def0d38d8fd5634c86e18110052bf3a8))
        (Set
         ("\001\255"
          .
          ./.
          ././.
          ..
          ../..
          .hidden
          "This is a sentence; it has punctuation, capitalization, and spaces!"
          bin
          bin/exe
          bin/exe/file
          filename.txt))
        (Map
         (("\001\255" 6)
          (. 0)
          (./. 7)
          (././. 9)
          (.. 1)
          (../.. 8)
          (.hidden 4)
          ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
          (bin 3)
          (bin/exe 10)
          (bin/exe/file 11)
          (filename.txt 2)))
        (Hash_set
         ("\001\255"
          .
          ./.
          ././.
          ..
          ../..
          .hidden
          "This is a sentence; it has punctuation, capitalization, and spaces!"
          bin
          bin/exe
          bin/exe/file
          filename.txt))
        (Table
         (("\001\255" 6)
          (. 0)
          (./. 7)
          (././. 9)
          (.. 1)
          (../.. 8)
          (.hidden 4)
          ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
          (bin 3)
          (bin/exe 10)
          (bin/exe/file 11)
          (filename.txt 2)))
        |}]
    ;;
  end
end

module Absolute = struct
  module V1 = struct
    type t = File_path.Stable.Absolute.V1.t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]

    type comparator_witness = File_path.Stable.Absolute.V1.comparator_witness

    let comparator = File_path.Stable.Absolute.V1.comparator
    let hashable = File_path.Stable.Absolute.V1.hashable

    let%expect_test _ =
      Helpers.test_stable_version
        [%here]
        (module File_path.Stable.Absolute.V1)
        Examples.Absolute.for_conversion;
      [%expect
        {|
        (bin_shape_digest e6c3839cfda3952ba5bf2f32724946b1)
        ((sexp /) (bin_io "\001/"))
        ((sexp /.) (bin_io "\002/."))
        ((sexp /..) (bin_io "\003/.."))
        ((sexp /filename.txt) (bin_io "\r/filename.txt"))
        ((sexp /bin) (bin_io "\004/bin"))
        ((sexp /.hidden) (bin_io "\b/.hidden"))
        ((sexp "/This is a sentence; it has punctuation, capitalization, and spaces!")
         (bin_io
          "D/This is a sentence; it has punctuation, capitalization, and spaces!"))
        ((sexp "/\001\255") (bin_io "\003/\001\255"))
        ((sexp /./.) (bin_io "\004/./."))
        ((sexp /../..) (bin_io "\006/../.."))
        ((sexp /././.) (bin_io "\006/././."))
        ((sexp /bin/exe) (bin_io "\b/bin/exe"))
        ((sexp /bin/exe/file) (bin_io "\r/bin/exe/file"))
        |}]
    ;;

    module Map = File_path.Stable.Absolute.V1.Map
    module Set = File_path.Stable.Absolute.V1.Set
    module Table = File_path.Stable.Absolute.V1.Table
    module Hash_set = File_path.Stable.Absolute.V1.Hash_set

    let%expect_test _ =
      Helpers.test_stable_containers
        [%here]
        (module File_path.Stable.Absolute.V1)
        Examples.Absolute.for_conversion;
      [%expect
        {|
        ((name Map) (bin_shape_digest e882f18d261f167f0b63dd0eda7789f2))
        ((name Set) (bin_shape_digest e15617a37fcb709b1970976b5bef6975))
        ((name Table) (bin_shape_digest d4f5b41e00a6adf84b206c072119e460))
        ((name Hash_set) (bin_shape_digest bb4918ef1705f54acc1cc9038c2c89c6))
        (Set
         (/
          "/\001\255"
          /.
          /./.
          /././.
          /..
          /../..
          /.hidden
          "/This is a sentence; it has punctuation, capitalization, and spaces!"
          /bin
          /bin/exe
          /bin/exe/file
          /filename.txt))
        (Map
         ((/ 0)
          ("/\001\255" 7)
          (/. 1)
          (/./. 8)
          (/././. 10)
          (/.. 2)
          (/../.. 9)
          (/.hidden 5)
          ("/This is a sentence; it has punctuation, capitalization, and spaces!" 6)
          (/bin 4)
          (/bin/exe 11)
          (/bin/exe/file 12)
          (/filename.txt 3)))
        (Hash_set
         (/
          "/\001\255"
          /.
          /./.
          /././.
          /..
          /../..
          /.hidden
          "/This is a sentence; it has punctuation, capitalization, and spaces!"
          /bin
          /bin/exe
          /bin/exe/file
          /filename.txt))
        (Table
         ((/ 0)
          ("/\001\255" 7)
          (/. 1)
          (/./. 8)
          (/././. 10)
          (/.. 2)
          (/../.. 9)
          (/.hidden 5)
          ("/This is a sentence; it has punctuation, capitalization, and spaces!" 6)
          (/bin 4)
          (/bin/exe 11)
          (/bin/exe/file 12)
          (/filename.txt 3)))
        |}]
    ;;
  end
end

module V1 = struct
  type t = File_path.Stable.V1.t
  [@@deriving
    bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]

  type comparator_witness = File_path.Stable.V1.comparator_witness

  let comparator = File_path.Stable.V1.comparator
  let hashable = File_path.Stable.V1.hashable

  let%expect_test _ =
    Helpers.test_stable_version
      [%here]
      (module File_path.Stable.V1)
      Examples.for_conversion;
    [%expect
      {|
      (bin_shape_digest f88be70577dfbd75687bcc492e1863e8)
      ((sexp .) (bin_io "\001."))
      ((sexp ..) (bin_io "\002.."))
      ((sexp filename.txt) (bin_io "\012filename.txt"))
      ((sexp bin) (bin_io "\003bin"))
      ((sexp .hidden) (bin_io "\007.hidden"))
      ((sexp "This is a sentence; it has punctuation, capitalization, and spaces!")
       (bin_io
        "CThis is a sentence; it has punctuation, capitalization, and spaces!"))
      ((sexp "\001\255") (bin_io "\002\001\255"))
      ((sexp ./.) (bin_io "\003./."))
      ((sexp ../..) (bin_io "\005../.."))
      ((sexp ././.) (bin_io "\005././."))
      ((sexp bin/exe) (bin_io "\007bin/exe"))
      ((sexp bin/exe/file) (bin_io "\012bin/exe/file"))
      ((sexp /) (bin_io "\001/"))
      ((sexp /.) (bin_io "\002/."))
      ((sexp /..) (bin_io "\003/.."))
      ((sexp /filename.txt) (bin_io "\r/filename.txt"))
      ((sexp /bin) (bin_io "\004/bin"))
      ((sexp /.hidden) (bin_io "\b/.hidden"))
      ((sexp "/This is a sentence; it has punctuation, capitalization, and spaces!")
       (bin_io
        "D/This is a sentence; it has punctuation, capitalization, and spaces!"))
      ((sexp "/\001\255") (bin_io "\003/\001\255"))
      ((sexp /./.) (bin_io "\004/./."))
      ((sexp /../..) (bin_io "\006/../.."))
      ((sexp /././.) (bin_io "\006/././."))
      ((sexp /bin/exe) (bin_io "\b/bin/exe"))
      ((sexp /bin/exe/file) (bin_io "\r/bin/exe/file"))
      |}]
  ;;

  module Map = File_path.Stable.V1.Map
  module Set = File_path.Stable.V1.Set
  module Table = File_path.Stable.V1.Table
  module Hash_set = File_path.Stable.V1.Hash_set

  let%expect_test _ =
    Helpers.test_stable_containers
      [%here]
      (module File_path.Stable.V1)
      Examples.for_conversion;
    [%expect
      {|
      ((name Map) (bin_shape_digest 2cdefe9add8dc26bf0a5d82cbb847d1c))
      ((name Set) (bin_shape_digest 1caa2335199b24a3dd64dfa6442c10d7))
      ((name Table) (bin_shape_digest 7e47d7a49368600574627a357b2139c8))
      ((name Hash_set) (bin_shape_digest 12d4ae0b344732bde6f68e7363ed5c52))
      (Set
       (/
        "/\001\255"
        /.
        /./.
        /././.
        /..
        /../..
        /.hidden
        "/This is a sentence; it has punctuation, capitalization, and spaces!"
        /bin
        /bin/exe
        /bin/exe/file
        /filename.txt
        "\001\255"
        .
        ./.
        ././.
        ..
        ../..
        .hidden
        "This is a sentence; it has punctuation, capitalization, and spaces!"
        bin
        bin/exe
        bin/exe/file
        filename.txt))
      (Map
       ((/ 12)
        ("/\001\255" 19)
        (/. 13)
        (/./. 20)
        (/././. 22)
        (/.. 14)
        (/../.. 21)
        (/.hidden 17)
        ("/This is a sentence; it has punctuation, capitalization, and spaces!" 18)
        (/bin 16)
        (/bin/exe 23)
        (/bin/exe/file 24)
        (/filename.txt 15)
        ("\001\255" 6)
        (. 0)
        (./. 7)
        (././. 9)
        (.. 1)
        (../.. 8)
        (.hidden 4)
        ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
        (bin 3)
        (bin/exe 10)
        (bin/exe/file 11)
        (filename.txt 2)))
      (Hash_set
       (/
        "/\001\255"
        /.
        /./.
        /././.
        /..
        /../..
        /.hidden
        "/This is a sentence; it has punctuation, capitalization, and spaces!"
        /bin
        /bin/exe
        /bin/exe/file
        /filename.txt
        "\001\255"
        .
        ./.
        ././.
        ..
        ../..
        .hidden
        "This is a sentence; it has punctuation, capitalization, and spaces!"
        bin
        bin/exe
        bin/exe/file
        filename.txt))
      (Table
       ((/ 12)
        ("/\001\255" 19)
        (/. 13)
        (/./. 20)
        (/././. 22)
        (/.. 14)
        (/../.. 21)
        (/.hidden 17)
        ("/This is a sentence; it has punctuation, capitalization, and spaces!" 18)
        (/bin 16)
        (/bin/exe 23)
        (/bin/exe/file 24)
        (/filename.txt 15)
        ("\001\255" 6)
        (. 0)
        (./. 7)
        (././. 9)
        (.. 1)
        (../.. 8)
        (.hidden 4)
        ("This is a sentence; it has punctuation, capitalization, and spaces!" 5)
        (bin 3)
        (bin/exe 10)
        (bin/exe/file 11)
        (filename.txt 2)))
      |}]
  ;;
end
