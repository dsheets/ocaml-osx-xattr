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

val get_size :
  ?no_follow:bool -> ?show_compression:bool -> string -> string -> int option

val get :
  ?no_follow:bool -> ?show_compression:bool -> ?size:int ->
  string -> string -> string option

val fget_size :
  ?show_compression:bool -> Unix.file_descr -> string -> int option

val fget :
  ?show_compression:bool -> ?size:int ->
  Unix.file_descr -> string -> string option

val list_size :
  ?no_follow:bool -> ?show_compression:bool ->
  string -> int

val list :
  ?no_follow:bool -> ?show_compression:bool -> ?size:int ->
  string -> string list

val flist_size : ?show_compression:bool -> Unix.file_descr -> int

val flist :
  ?show_compression:bool -> ?size:int -> Unix.file_descr -> string list

val set :
  ?no_follow:bool -> ?create:bool -> ?replace:bool ->
  string -> string -> string -> unit

val fset :
  ?create:bool -> ?replace:bool -> Unix.file_descr -> string -> string -> unit

val remove :
  ?no_follow:bool -> ?show_compression:bool ->
  string -> string -> unit

val fremove : ?show_compression:bool -> Unix.file_descr -> string -> unit
