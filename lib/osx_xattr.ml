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
module C = Osx_xattr_bindings.C(Osx_xattr_generated)

let int_of_fd = Unix_representations.int_of_file_descr

let get_size ?(no_follow=false) ?(show_compression=false) path name =
  try
    let size =
      Errno_unix.raise_on_errno ~call:"getxattr" ~label:name (fun () ->
        C.get path name null (Unsigned.Size_t.of_int 0)
          Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
      )
    in
    Some (Int64.to_int (PosixTypes.Ssize.to_int64 size))
  with Errno.Error { Errno.errno } as exn ->
    if List.mem (Errno.EUNKNOWNERR 93) errno
    (* TODO: update unix-errno with ENOATTR/ENODATA *)
    then None
    else raise exn

(* TODO: provide a supplied-buffer+offset version? *)

let get ?(no_follow=false) ?(show_compression=false) path name =
  let rec call count =
    let buf = allocate_n char ~count in
    try
      let read =
        Errno_unix.raise_on_errno ~call:"getxattr" ~label:name (fun () ->
          C.get path name (to_voidp buf) (Unsigned.Size_t.of_int count)
            Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
        )
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      Some (string_from_ptr buf ~length:read)
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match get_size ~no_follow ~show_compression path name with
        | Some size -> call size
        | None -> None
      else if List.mem (Errno.EUNKNOWNERR 93) errno
      (* TODO: update unix-errno with ENOATTR/ENODATA *)
      then None
      else raise exn
  in
  call 256

let fget_size ?(no_follow=false) ?(show_compression=false) fd name =
  try
    let size =
      Errno_unix.raise_on_errno ~call:"fgetxattr" ~label:name (fun () ->
        C.fget (int_of_fd fd) name null (Unsigned.Size_t.of_int 0)
          Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
      )
    in
    Some (Int64.to_int (PosixTypes.Ssize.to_int64 size))
  with Errno.Error { Errno.errno } as exn ->
    if List.mem (Errno.EUNKNOWNERR 93) errno
    (* TODO: update unix-errno with ENOATTR/ENODATA *)
    then None
    else raise exn

(* TODO: provide a supplied-buffer+offset version? *)

let fget ?(no_follow=false) ?(show_compression=false) fd name =
  let rec call count =
    let buf = allocate_n char ~count in
    try
      let read =
        Errno_unix.raise_on_errno ~call:"fgetxattr" ~label:name (fun () ->
          C.fget (int_of_fd fd) name (to_voidp buf)
            (Unsigned.Size_t.of_int count)
            Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
        )
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      Some (string_from_ptr buf ~length:read)
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match fget_size ~no_follow ~show_compression fd name with
        | Some size -> call size
        | None -> None
      else if List.mem (Errno.EUNKNOWNERR 93) errno
      (* TODO: update unix-errno with ENOATTR/ENODATA *)
      then None
      else raise exn
  in
  call 256

let rec list_of_strings_buffer acc buf = function
  | 0 -> List.rev acc
  | sz when sz < 0 -> assert false
  | sz ->
    let next = coerce (ptr char) string buf in
    let len = String.length next in
    assert (len < sz);
    list_of_strings_buffer (next::acc) (buf +@ (len + 1)) (sz - len - 1)

let list_size ?(no_follow=false) ?(show_compression=false) path =
  let size =
    Errno_unix.raise_on_errno ~call:"listxattr" ~label:path (fun () ->
      C.list path null (Unsigned.Size_t.of_int 0)
        { C.GetOptions.no_follow; show_compression }
    )
  in
  Int64.to_int (PosixTypes.Ssize.to_int64 size)

(* TODO: provide a supplied-buffer+offset version? *)

let list ?(no_follow=false) ?(show_compression=false) path =
  let rec call count =
    let buf = allocate_n char ~count in
    try
      let read =
        Errno_unix.raise_on_errno ~call:"listxattr" ~label:path (fun () ->
          C.list path (to_voidp buf) (Unsigned.Size_t.of_int count)
            { C.GetOptions.no_follow; show_compression }
        )
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      list_of_strings_buffer [] buf read
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match list_size ~no_follow ~show_compression path with
        | 0 -> []
        | size -> call size
      else raise exn
  in
  call 256

let flist_size ?(no_follow=false) ?(show_compression=false) fd =
  let fd = int_of_fd fd in
  let label = string_of_int fd in
  let size =
    Errno_unix.raise_on_errno ~call:"flistxattr" ~label (fun () ->
      C.flist fd null (Unsigned.Size_t.of_int 0)
        { C.GetOptions.no_follow; show_compression }
    )
  in
  Int64.to_int (PosixTypes.Ssize.to_int64 size)

(* TODO: provide a supplied-buffer+offset version? *)

let flist ?(no_follow=false) ?(show_compression=false) fd =
  let rec call count =
    let buf = allocate_n char ~count in
    try
      let fd = int_of_fd fd in
      let label = string_of_int fd in
      let read =
        Errno_unix.raise_on_errno ~call:"flistxattr" ~label (fun () ->
          C.flist fd (to_voidp buf)
            (Unsigned.Size_t.of_int count)
            { C.GetOptions.no_follow; show_compression }
        )
      in
      let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
      list_of_strings_buffer [] buf read
    with Errno.Error { Errno.errno } as exn ->
      if List.mem Errno.ERANGE errno
      then match flist_size ~no_follow ~show_compression fd with
        | 0 -> []
        | size -> call size
      else raise exn
  in
  call 256

(* TODO: Can raise unbound ENOATTR *)
let set ?(no_follow=false) ?(create=false) ?(replace=false) path name value =
  let size = Unsigned.Size_t.of_int (String.length value) in
  Errno_unix.raise_on_errno ~call:"setxattr" ~label:name (fun () ->
    ignore
      (C.set path name (ocaml_string_start value) size Unsigned.UInt32.zero
         { C.SetOptions.no_follow; create; replace })
  )

(* TODO: Can raise unbound ENOATTR *)
let fset ?(no_follow=false) ?(create=false) ?(replace=false) fd name value =
  let size = Unsigned.Size_t.of_int (String.length value) in
  let fd = int_of_fd fd in
  Errno_unix.raise_on_errno ~call:"fsetxattr" ~label:name (fun () ->
    ignore
      (C.fset fd name (ocaml_string_start value) size Unsigned.UInt32.zero
         { C.SetOptions.no_follow; create; replace })
  )

(* TODO: Can raise unbound ENOATTR *)
let remove ?(no_follow=false) ?(show_compression=false) path name =
  Errno_unix.raise_on_errno ~call:"removexattr" ~label:name (fun () ->
    ignore (C.remove path name { C.GetOptions.no_follow; show_compression })
  )

(* TODO: Can raise unbound ENOATTR *)
let fremove ?(no_follow=false) ?(show_compression=false) fd name =
  let fd = int_of_fd fd in
  Errno_unix.raise_on_errno ~call:"fremovexattr" ~label:name (fun () ->
    ignore (C.fremove fd name { C.GetOptions.no_follow; show_compression })
  )
