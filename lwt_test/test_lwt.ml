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

open Lwt.Infix

module Basic = struct
  let test_file = "__test_xattr"
  
  let state = ref None

  let setup () = match !state with
    | Some fd -> fd
    | None ->
      (try Unix.unlink test_file
       with Unix.Unix_error (Unix.ENOENT, "unlink", _) -> ());
      let fd = Unix.(openfile test_file [O_CREAT] 0o600) in
      state := Some fd;
      fd

  let cleanup () =
    Unix.unlink test_file

  let set_get () =
    let _fd = setup () in
    Lwt_main.run (
      let attr = "set_get" in
      let value = "set_get value" in
      Osx_xattr_lwt.set test_file attr value >>= fun () ->
      Osx_xattr_lwt.get test_file attr >>= function
      | Some value' -> Lwt.return (Alcotest.(check string) "set_get" value value')
      | None -> Alcotest.fail "couldn't get xattr"
    )

  let fset_fget () =
    let fd = setup () in
    Lwt_main.run (
      let attr = "fset_fget" in
      let value = "fset_fget value" in
      Osx_xattr_lwt.fset fd attr value >>= fun () ->
      Osx_xattr_lwt.fget fd attr >>= function
      | Some value' -> Lwt.return (Alcotest.(check string) "fset_fget" value value')
      | None -> Alcotest.fail "couldn't get xattr"
    )

  let list_remove () =
    let _fd = setup () in
    Lwt_main.run (
      (* TODO: make this test stand-alone *)
      Osx_xattr_lwt.list test_file >>= fun xattrs ->
      Alcotest.(check (list string)) "list_remove before"
        (List.sort String.compare ["set_get"; "fset_fget"])
        (List.sort String.compare xattrs);
      Osx_xattr_lwt.remove test_file "set_get" >>= fun () ->
      Osx_xattr_lwt.list test_file >>= fun xattrs ->
      Alcotest.(check (list string)) "list_remove after" ["fset_fget"] xattrs;
      Lwt.return_unit
    )

  let flist_fremove () =
    let fd = setup () in
    Lwt_main.run (
      (* TODO: make this test stand-alone *)
      Osx_xattr_lwt.flist fd >>= fun xattrs ->
      Alcotest.(check (list string)) "flist_fremove before" ["fset_fget"] xattrs;
      Osx_xattr_lwt.fremove fd "fset_fget" >>= fun () ->
      Osx_xattr_lwt.flist fd >>= fun xattrs ->
      Alcotest.(check (list string)) "flist_fremove after" [] xattrs;
      (* TODO: This should happen after all tests *)
      cleanup ();
      Lwt.return_unit
    )

  let tests = [
    "set_get",       `Quick, set_get;
    "fset_fget",     `Quick, fset_fget;
    "list_remove",   `Quick, list_remove;
    "flist_fremove", `Quick, flist_fremove;
  ]
end

let tests = [
  "Basic", Basic.tests;
]

let cleanup () =
  Basic.cleanup ()

;;
Alcotest.run "OSX xattr" tests
