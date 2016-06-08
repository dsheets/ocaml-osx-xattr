(*
 * Copyright (c) 2016 David Sheets <dsheets@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Ctypes

module Types = Osx_xattr_types.C(Osx_xattr_types_detected)
module C = Osx_xattr_bindings.C
    (struct
      include Osx_xattr_generated
      let foreign f = foreign ("osx_xattr_" ^ f)
    end)

let int_of_fd = Unix_representations.int_of_file_descr

let get_size ?(no_follow=false) ?(show_compression=false) path name =
  try Errno_unix.raise_on_errno ~call:"getxattr" ~label:name (fun () ->
    let size = C.get path name null (Unsigned.Size_t.of_int 0)
        Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
    in
    let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
    if size < 0 then None else Some (Some size)
  )
  with Errno.Error { Errno.errno } as exn ->
    if List.mem Errno.ENOATTR errno
    then None
    else raise exn

let get ?(no_follow=false) ?(show_compression=false) ?(size=64) path name =
  let rec call count =
    let buf = allocate_n char ~count in
    try Errno_unix.raise_on_errno ~call:"getxattr" ~label:name (fun () ->
      let read = C.get path name (to_voidp buf) (Unsigned.Size_t.of_int count)
          Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      if read < 0 then None else Some (Some (string_from_ptr buf ~length:read))
    )
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match get_size ~no_follow ~show_compression path name with
        | Some size -> call size
        | None -> None
      else if List.mem Errno.ENOATTR errno
      then None
      else raise exn
  in
  call size

let fget_size ?(show_compression=false) fd name =
  try Errno_unix.raise_on_errno ~call:"fgetxattr" ~label:name (fun () ->
    let size = C.fget (int_of_fd fd) name null (Unsigned.Size_t.of_int 0)
        Unsigned.UInt32.zero { C.GetOptions.no_follow=false; show_compression }
    in
    let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
    if size < 0 then None else Some (Some size)
  )
  with Errno.Error { Errno.errno } as exn ->
    if List.mem Errno.ENOATTR errno
    then None
    else raise exn

let fget ?(show_compression=false) ?(size=64) fd name =
  let rec call count =
    let buf = allocate_n char ~count in
    try Errno_unix.raise_on_errno ~call:"fgetxattr" ~label:name (fun () ->
      let read = C.fget (int_of_fd fd) name (to_voidp buf)
          (Unsigned.Size_t.of_int count)
          Unsigned.UInt32.zero
          { C.GetOptions.no_follow=false; show_compression }
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      if read < 0 then None else Some (Some (string_from_ptr buf ~length:read))
    )
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match fget_size ~show_compression fd name with
        | Some size -> call size
        | None -> None
      else if List.mem Errno.ENOATTR errno
      then None
      else raise exn
  in
  call size

let rec list_of_strings_buffer acc buf = function
  | 0 -> List.rev acc
  | sz when sz < 0 -> assert false
  | sz ->
    let next = coerce (ptr char) string buf in
    let len = String.length next in
    assert (len < sz);
    list_of_strings_buffer (next::acc) (buf +@ (len + 1)) (sz - len - 1)

let list_size ?(no_follow=false) ?(show_compression=false) path =
  Errno_unix.raise_on_errno ~call:"listxattr" ~label:path (fun () ->
    let size = C.list path null (Unsigned.Size_t.of_int 0)
        { C.GetOptions.no_follow; show_compression }
    in
    let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
    if size < 0 then None else Some size
  )

let list ?(no_follow=false) ?(show_compression=false) ?(size=64) path =
  let rec call count =
    let buf = allocate_n char ~count in
    try Errno_unix.raise_on_errno ~call:"listxattr" ~label:path (fun () ->
      let read = C.list path (to_voidp buf) (Unsigned.Size_t.of_int count)
          { C.GetOptions.no_follow; show_compression }
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      if read < 0 then None else Some (list_of_strings_buffer [] buf read)
    )
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match list_size ~no_follow ~show_compression path with
        | 0 -> []
        | size -> call size
      else raise exn
  in
  call size

let flist_size ?(show_compression=false) fd =
  let fd = int_of_fd fd in
  let label = string_of_int fd in
  Errno_unix.raise_on_errno ~call:"flistxattr" ~label (fun () ->
    let size = C.flist fd null (Unsigned.Size_t.of_int 0)
        { C.GetOptions.no_follow=false; show_compression }
    in
    let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
    if size < 0 then None else Some size
  )

let flist ?(show_compression=false) ?(size=64) fd =
  let rec call count =
    let buf = allocate_n char ~count in
    try
      let fd = int_of_fd fd in
      let label = string_of_int fd in
      Errno_unix.raise_on_errno ~call:"flistxattr" ~label (fun () ->
        let read = C.flist fd (to_voidp buf)
            (Unsigned.Size_t.of_int count)
            { C.GetOptions.no_follow=false; show_compression }
        in
        let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
        if read < 0 then None else Some (list_of_strings_buffer [] buf read)
      )
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match flist_size ~show_compression fd with
        | 0 -> []
        | size -> call size
      else raise exn
  in
  call size

let set ?(no_follow=false) ?(create=false) ?(replace=false) path name value =
  let size = Unsigned.Size_t.of_int (String.length value) in
  Errno_unix.raise_on_errno ~call:"setxattr" ~label:name (fun () ->
    let rc =
      C.set path name value size Unsigned.UInt32.zero
        { C.SetOptions.no_follow; create; replace }
    in
    if rc < 0 then None else Some ()
  )

let fset ?(create=false) ?(replace=false) fd name value =
  let size = Unsigned.Size_t.of_int (String.length value) in
  let fd = int_of_fd fd in
  Errno_unix.raise_on_errno ~call:"fsetxattr" ~label:name (fun () ->
    let rc =
      C.fset fd name value size Unsigned.UInt32.zero
        { C.SetOptions.no_follow=false; create; replace }
    in
    if rc < 0 then None else Some ()
  )

let remove ?(no_follow=false) ?(show_compression=false) path name =
  Errno_unix.raise_on_errno ~call:"removexattr" ~label:name (fun () ->
    let rc = C.remove path name { C.GetOptions.no_follow; show_compression } in
    if rc < 0 then None else Some ()
  )

let fremove ?(show_compression=false) fd name =
  let fd = int_of_fd fd in
  Errno_unix.raise_on_errno ~call:"fremovexattr" ~label:name (fun () ->
    let rc =
      C.fremove fd name { C.GetOptions.no_follow=false; show_compression }
    in
    if rc < 0 then None else Some ()
  )
