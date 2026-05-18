# File_path

`File_path` is a library for typed manipulation of UNIX-style file paths.
It performs string manipulations only; there is no I/O and the file system
is not checked for file existence, symlinks, etc.

# Types

The library distinguishes among four kinds of path strings. All paths
are stored with duplicate and trailing slashes removed.

A `File_path.t` is an arbitrary path.

```ocaml
# File_path.of_string "a//relative//path"
- : File_path.t = a/relative/path
# File_path.of_string "/an/absolute/path/"
- : File_path.t = /an/absolute/path
```

A `File_path.Relative.t` is a relative path. It may not start with a slash.

```ocaml
# File_path.Relative.of_string "a//relative//path"
- : File_path.Relative.t = a/relative/path
# File_path.Relative.of_string "/an/absolute/path/"
Exception:
("File_path.Relative.of_string: invalid string" /an/absolute/path/)
```

A `File_path.Absolute.t` is an absolute path. It must start with a slash.

```ocaml
# File_path.Absolute.of_string "/an/absolute/path/"
- : File_path.Absolute.t = /an/absolute/path
# File_path.Absolute.of_string "a//relative//path"
Exception: ("File_path.Absolute.of_string: invalid string" a//relative//path)
```

A `File_path.Part.t` is a single name with no slashes.

```ocaml
# File_path.Part.of_string "here"
- : File_path.Part.t = here
# File_path.Part.of_string "a//relative//path"
Exception: ("File_path.Part.of_string: invalid string" a//relative//path)
# File_path.Part.of_string "/an/absolute/path/"
Exception: ("File_path.Part.of_string: invalid string" /an/absolute/path/)
```

All of these types are exposed as `type t = private string`, meaning
they can be safely cast to a `string` for ease of printing or
conversion without needing to call a conversion function.

# Constants

The special paths `.`, `..`, and `/` are predefined for relevant types.

```ocaml
# File_path.dot, File_path.dot_dot, File_path.root
- : File_path.t * File_path.t * File_path.t = (., .., /)
# File_path.Relative.dot, File_path.Relative.dot_dot
- : File_path.Relative.t * File_path.Relative.t = (., ..)
# File_path.Absolute.root
- : File_path.Absolute.t = /
# File_path.Part.dot, File_path.Part.dot_dot
- : File_path.Part.t * File_path.Part.t = (., ..)
```

# Identifiable

All path types provide `Identifiable.S`.

All `to_string` functions are the identity.

The `of_string` functions check for valid path strings---e.g., no null
characters---and remove duplicate and trailing slashes. When the input
is already canonical, the `of_string` functions return the input
itself without allocating a new string.

# Command-line Arguments

All path types provide an `arg_type` that supports autocompletion.

# I/O Operations

For I/O operations, see `Filesystem_core` and `Filesystem_async`.
These modules provide equivalent operations satisfying
`Filesystem_intf.S`, differing only in whether they are blocking or
asynchronous. They largely mirror file operations in `Sys` and `Unix`,
plus reading and writing whole files.

These operations mostly use `File_path.t`. Unless specified otherwise,
paths are interpreted relative to the current working directory.

# Usage

Here are some examples of using `File_path` operations. For operations
that can fail, we provide a version that returns an option, a version
that may raise, and a version that returns a default value. Where
applicable, `File_path.Absolute` and `File_path.Relative` support the
operations of `File_path`.

Splitting up a path:
```ocaml
# File_path.dirname_and_basename File_path.root
- : (File_path.t * File_path.Part.t) option = None
# File_path.dirname_and_basename File_path.dot
- : (File_path.t * File_path.Part.t) option = None
# File_path.dirname_and_basename (File_path.of_string "this/is/a/path")
- : (File_path.t * File_path.Part.t) option = Some (this/is/a, path)
```

Splitting up a path without needing options or exceptions:
```ocaml
# File_path.dirname_defaulting_to_dot_or_root File_path.root
- : File_path.t = /
# File_path.basename_defaulting_to_dot File_path.root
- : File_path.Part.t = .
```

Appending paths:
```ocaml
# File_path.append
    (File_path.of_string "/absolute")
    (File_path.Relative.of_string "directory/tree")
- : File_path.t = /absolute/directory/tree
# File_path.append
    (File_path.of_string "relative")
    (File_path.Relative.of_string "directory/tree")
- : File_path.t = relative/directory/tree
```

Chopping paths:
```ocaml
# File_path.chop_prefix_exn
    (File_path.of_string "/usr/local/home/me/bin/script.sh")
    ~prefix:(File_path.of_string "/usr/local/home/me")
- : File_path.Relative.t = bin/script.sh
# File_path.chop_prefix_if_exists
    (File_path.of_string "/usr/bin/grep")
    ~prefix:(File_path.of_string "/usr/local/home/me")
- : File_path.t = /usr/bin/grep
```

Converting paths:
```ocaml
# File_path.make_absolute
    (File_path.of_string "bin/script.sh")
    ~under:(File_path.Absolute.of_string "/usr/local/home/me")
- : File_path.Absolute.t = /usr/local/home/me/bin/script.sh
# File_path.make_relative_if_possible
    (File_path.of_string "/usr/bin/grep")
    ~if_under:(File_path.Absolute.of_string "/usr/local/home/me")
- : File_path.t = /usr/bin/grep
```

Assembling and disassembling paths:
```ocaml
# let parts = File_path.to_parts (File_path.of_string "/usr/local/share/dict/words")
val parts : File_path.Part.t list = [usr; local; share; dict; words]
# File_path.of_parts_absolute parts
- : File_path.t = /usr/local/share/dict/words
```

Simplifying paths, without access to the filesystem:
```ocaml
# let path = File_path.of_string "./path/etc/../bin/."
val path : File_path.t = ./path/etc/../bin/.
# File_path.simplify_dot path
- : File_path.t = path/etc/../bin
# File_path.simplify_dot_and_dot_dot_naively path
- : File_path.t = path/bin
```

# Shorthand

For convenience, some infix and prefix operators are provided.

```ocaml
open File_path.Operators
```

Operators for path construction:
```ocaml
# ~/"this/is/a/relative/path"
- : File_path.Relative.t = this/is/a/relative/path
# !/"/this/is/an/absolute/path"
- : File_path.Absolute.t = /this/is/an/absolute/path
# ?/"this/is/a/path"
- : File_path.t = this/is/a/path
# ~."path-part"
- : File_path.Part.t = path-part
```

Operators for path concatenation:
```ocaml
# ~/"rel/path" /~/ ~/"sub/dir"
- : File_path.Relative.t = rel/path/sub/dir
# !/"/abs/path" /!/ ~/"sub/dir"
- : File_path.Absolute.t = /abs/path/sub/dir
# ?/"/path" /?/ ~/"sub/dir"
- : File_path.t = /path/sub/dir
```

Operators for path part concatenation:
```ocaml
# ~/"rel/path" /~. ~."sub" /~. ~."dir"
- : File_path.Relative.t = rel/path/sub/dir
# !/"/abs/path" /!. ~."sub" /!. ~."dir"
- : File_path.Absolute.t = /abs/path/sub/dir
# ?/"/path" /?. ~."sub" /?. ~."dir"
- : File_path.t = /path/sub/dir
```

Operators for file extension concatenation:
```ocaml
# ~/"path/to/file" /~^ ".ml" /~^ ".tmp"
- : File_path.Relative.t = path/to/file.ml.tmp
# !/"/path/to/file" /!^ ".ml" /!^ ".tmp"
- : File_path.Absolute.t = /path/to/file.ml.tmp
# ?/"/path/to/file" /?^ ".ml" /?^ ".tmp"
- : File_path.t = /path/to/file.ml.tmp
# ~."file" /.^ ".ml" /.^ ".tmp"
- : File_path.Part.t = file.ml.tmp
```
