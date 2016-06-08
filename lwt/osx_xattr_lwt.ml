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
module Generated = Osx_xattr_lwt_generated
module C = Osx_xattr_bindings.C(Generated)

open Lwt.Infix

let int_of_fd = Unix_representations.int_of_file_descr
let errno_of_code code = Errno.of_code ~host:Errno_unix.host code

let handle_errno ~call ~label errno return =
  let errnos = errno_of_code errno in
  if List.mem Errno.ENOATTR errnos then Lwt.return return
  else raise (Errno.Error {
      Errno.errno = errnos; call; label;
    })
  
let get_size ?(no_follow=false) ?(show_compression=false) path name =
  let call =
    C.get path name null (Unsigned.Size_t.of_int 0)
      Unsigned.UInt32.zero { C.GetOptions.no_follow; show_compression }
  in
  call.Generated.lwt >>= fun (size, errno) ->
  let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
  if size >= 0 then Lwt.return_some size
  else handle_errno ~call:"getxattr" ~label:name errno None

let get ?(no_follow=false) ?(show_compression=false) ?(size=64) path name =
  let rec call count =
    let buf = allocate_n char ~count in
    (C.get path name (to_voidp buf) (Unsigned.Size_t.of_int count)
       Unsigned.UInt32.zero
       { C.GetOptions.no_follow; show_compression }).Generated.lwt >>= fun (read, errno) ->
    let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
    if read < 0 then
      let errnos = errno_of_code errno in
      if List.mem Errno.ERANGE errnos
      then get_size ~no_follow ~show_compression path name >>= function
        | Some size -> call size
        | None -> Lwt.return_none
      else if List.mem Errno.ENOATTR errnos then Lwt.return_none
      else raise (Errno.Error {
      Errno.errno = errnos; call = "getxattr"; label = name;
        })
    else Lwt.return_some (string_from_ptr buf ~length:read)
  in
  call size

let fget_size ?(show_compression=false) fd name =
  let call =
    C.fget (int_of_fd fd) name null (Unsigned.Size_t.of_int 0)
      Unsigned.UInt32.zero { C.GetOptions.no_follow=false; show_compression }
  in
  call.Generated.lwt >>= fun (size, errno) ->
  let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
  if size >= 0 then Lwt.return_some size
  else handle_errno ~call:"fgetxattr" ~label:name errno None

let fget ?(show_compression=false) ?(size=64) fd name =
  let rec call count =
    let buf = allocate_n char ~count in
    (C.fget (int_of_fd fd) name (to_voidp buf)
       (Unsigned.Size_t.of_int count)
       Unsigned.UInt32.zero
       { C.GetOptions.no_follow=false; show_compression }).Generated.lwt >>= fun (read, errno) ->
    let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
    if read < 0 then
      let errnos = errno_of_code errno in
      if List.mem Errno.ERANGE errnos
      then fget_size ~show_compression fd name >>= function
        | Some size -> call size
        | None -> Lwt.return_none
      else if List.mem Errno.ENOATTR errnos then Lwt.return_none
      else raise (Errno.Error {
      Errno.errno = errnos; call = "getxattr"; label = name;
        })
    else Lwt.return_some (string_from_ptr buf ~length:read)
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
  (C.list path null (Unsigned.Size_t.of_int 0)
     { C.GetOptions.no_follow; show_compression }).Generated.lwt >>= fun (size, errno) ->
  let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in
  if size < 0 then raise (Errno.Error {
      Errno.errno = errno_of_code errno; call = "listxattr"; label = path;
    })
  else Lwt.return size

let list ?(no_follow=false) ?(show_compression=false) ?(size=64) path =
  let rec call count =
    let buf = allocate_n char ~count in
    (C.list path (to_voidp buf) (Unsigned.Size_t.of_int count)
       { C.GetOptions.no_follow; show_compression }).Generated.lwt >>= fun (read, errno) ->
    let read = Int64.to_int (PosixTypes.Ssize.to_int64 read) in
    if read < 0 then 
      let errnos = errno_of_code errno in
      if List.mem Errno.ERANGE errnos
      then list_size ~no_follow ~show_compression path >>= function
        | 0 -> Lwt.return []
        | size -> call size
      else raise (Errno.Error {Errno.errno = errnos; call = "flistxattr"; label = path; })
    else Lwt.return (list_of_strings_buffer [] buf read)
  in
  call size

let flist_size ?(show_compression=false) fd =
  let fd = int_of_fd fd in
  let label = string_of_int fd in
  (C.flist fd null (Unsigned.Size_t.of_int 0)
     { C.GetOptions.no_follow=false; show_compression }).Generated.lwt >>= fun (size, errno) ->
  let size = Int64.to_int (PosixTypes.Ssize.to_int64 size) in  
  if size < 0
  then raise (Errno.Error {Errno.errno = errno_of_code errno; call = "flistxattr"; label; })
  else Lwt.return size
