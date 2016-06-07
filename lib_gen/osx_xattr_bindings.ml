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

let (|||) = (lor)

let (??>) flag int = if flag then int else 0

let (??<) field int = field land int <> 0

module Type = Osx_xattr_types.C(Osx_xattr_types_detected)

module C(F: Cstubs.FOREIGN) = struct

  module GetOptions = struct

    type t = {
      no_follow        : bool;
      show_compression : bool;
    }

    let empty = {
      no_follow        = false;
      show_compression = false;
     }

    let to_int {
      no_follow;
      show_compression;
    } =
      (??> no_follow        Type.Options.nofollow) |||
      (??> show_compression Type.Options.showcompression)

    let of_int i = {
      no_follow        = ??< i Type.Options.nofollow;
      show_compression = ??< i Type.Options.showcompression;
    }

    let t = view ~read:of_int ~write:to_int int

  end

  module SetOptions = struct

    type t = {
      no_follow : bool;
      create    : bool;
      replace   : bool;
    }

    let empty = {
      no_follow = false;
      create    = false;
      replace   = false;
     }

    let to_int {
      no_follow;
      create;
      replace;
    } =
      (??> no_follow Type.Options.nofollow) |||
      (??> create    Type.Options.create) |||
      (??> replace   Type.Options.replace)

    let of_int i = {
      no_follow = ??< i Type.Options.nofollow;
      create    = ??< i Type.Options.create;
      replace   = ??< i Type.Options.replace;
    }

    let t = view ~read:of_int ~write:to_int int

  end

  let get = F.(foreign "getxattr" (
    string @-> string @-> ptr void @-> size_t @-> uint32_t @->
    GetOptions.t @-> returning PosixTypes.ssize_t
  ))

  let fget = F.(foreign "fgetxattr" (
    int @-> string @-> ptr void @-> size_t @-> uint32_t @->
    GetOptions.t @-> returning PosixTypes.ssize_t
  ))

  let list = F.(foreign "listxattr" (
    string @-> ptr void @-> size_t @-> GetOptions.t @->
    returning PosixTypes.ssize_t
  ))

  let flist = F.(foreign "flistxattr" (
    int @-> ptr void @-> size_t @-> GetOptions.t @->
    returning PosixTypes.ssize_t
  ))

  let set = F.(foreign "setxattr" (
    string @-> string @-> string @-> size_t @-> uint32_t @->
    SetOptions.t @-> returning int
  ))

  let fset = F.(foreign "fsetxattr" (
    int @-> string @-> string @-> size_t @-> uint32_t @->
    SetOptions.t @-> returning int
  ))

  let remove = F.(foreign "removexattr" (
    string @-> string @-> GetOptions.t @-> returning int
  ))

  let fremove = F.(foreign "fremovexattr" (
    int @-> string @-> GetOptions.t @-> returning int
  ))
end
