open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

let ctypes_libdir = Sys.getenv "CTYPES_LIB_DIR" in
let lwt_libdir = Sys.getenv "LWT_LIB_DIR" in
let ocaml_libdir = Sys.getenv "OCAML_LIB_DIR" in

dispatch begin
  function
  | After_rules ->

    rule "cstubs: lib_gen/x_types_detect.c -> x_types_detect"
      ~prods:["lib_gen/%_types_detect"]
      ~deps:["lib_gen/%_types_detect.c"]
      (fun env build ->
         Cmd (S[A"cc";
                A("-I"); A ctypes_libdir;
                A("-I"); A ocaml_libdir;
                A"-o";
                A(env "lib_gen/%_types_detect");
                A(env "lib_gen/%_types_detect.c");
               ]));

    rule "cstubs: lib_gen/x_types_detect -> lib/x_types_detected.ml"
      ~prods:["lib/%_types_detected.ml"]
      ~deps:["lib_gen/%_types_detect"]
      (fun env build ->
         Cmd (S[A(env "lib_gen/%_types_detect");
                Sh">";
                A(env "lib/%_types_detected.ml");
               ]));

    rule "cstubs: lib_gen/x_types.ml -> x_types_detect.c"
      ~prods:["lib_gen/%_types_detect.c"]
      ~deps: ["lib_gen/%_typegen.byte"]
      (fun env build ->
         Cmd (A(env "lib_gen/%_typegen.byte")));

    copy_rule "cstubs: lib_gen/x_types.ml -> lib/x_types.ml"
      "lib_gen/%_types.ml" "lib/%_types.ml";

    rule "cstubs: lib/x_bindings.ml -> x_stubs.c, x_generated.ml"
      ~prods:["lib/%_stubs.c"; "lib/%_generated.ml"]
      ~deps: ["lib_gen/%_bindgen.byte"]
      (fun env build ->
        Cmd (S[A(env "lib_gen/%_bindgen.byte");
               A"--c-file";
               A(env "lib/%_stubs.c");
                 A"--ml-file";
               A(env "lib/%_generated.ml")]));

    rule "cstubs: lwt/x_bindings.ml -> x_stubs.c, x_generated.ml"
      ~prods:["lwt/%_lwt_stubs.c"; "lwt/%_lwt_generated.ml"]
      ~deps: ["lib_gen/%_bindgen.byte"]
      (fun env build ->
        Cmd (S[A(env "lib_gen/%_bindgen.byte");
               A"--lwt-bindings";
               A"--c-file";
               A(env "lwt/%_lwt_stubs.c");
                 A"--ml-file";
               A(env "lwt/%_lwt_generated.ml")]));

    copy_rule "cstubs: lib_gen/x_bindings.ml -> lib/x_bindings.ml"
      "lib_gen/%_bindings.ml" "lib/%_bindings.ml";

    flag ["c"; "compile"] & S[A"-ccopt"; A"-I/usr/local/include"];
    flag ["c"; "ocamlmklib"] & S[A"-L/usr/local/lib"];
    flag ["ocaml"; "link"; "native"; "program"] &
      S[A"-cclib"; A"-L/usr/local/lib";
        A"-cclib"; A("-L"^Unix.getcwd()^"/_build/lib");
        A"-cclib"; A("-L"^Unix.getcwd()^"/_build/lwt")];

    (* Linking cstubs *)
    dep ["c"; "compile"; "use_osx_xattr_util"]
      ["lib/osx_xattr_util.o"; "lib/osx_xattr_util.h"];
    flag ["c"; "compile"; "use_ctypes"] & S[A"-I"; A ctypes_libdir];
    flag ["c"; "compile"; "use_lwt"] & S[A"-I"; A lwt_libdir];
    flag ["c"; "compile"; "debug"] & A"-g";

    (* Linking generated stubs *)
    dep ["ocaml"; "link"; "byte"; "library"; "use_osx_xattr_stubs"]
      ["lib/dllosx_xattr_stubs"-.-(!Options.ext_dll)];
    flag ["ocaml"; "link"; "byte"; "library"; "use_osx_xattr_stubs"] &
    S[A"-dllib"; A"-losx_xattr_stubs"];

    dep ["ocaml"; "link"; "native"; "library"; "use_osx_xattr_stubs"]
      ["lib/libosx_xattr_stubs"-.-(!Options.ext_lib)];
    flag ["ocaml"; "link"; "native"; "library"; "use_osx_xattr_stubs"] &
    S[A"-cclib"; A"-losx_xattr_stubs"];

    (* Linking generated lwt stubs *)
    dep ["ocaml"; "link"; "byte"; "library"; "use_osx_xattr_lwt_stubs"]
      ["lwt/dllosx_xattr_lwt_stubs"-.-(!Options.ext_dll)];
    flag ["ocaml"; "link"; "byte"; "library"; "use_osx_xattr_lwt_stubs"] &
    S[A"-dllib"; A"-losx_xattr_lwt_stubs"];

    dep ["ocaml"; "link"; "native"; "library"; "use_osx_xattr_lwt_stubs"]
      ["lwt/libosx_xattr_lwt_stubs"-.-(!Options.ext_lib)];
    flag ["ocaml"; "link"; "native"; "library"; "use_osx_xattr_lwt_stubs"] &
    S[A"-cclib"; A"-losx_xattr_lwt_stubs"];

    (* Linking tests *)
    flag ["ocaml"; "link"; "byte"; "program"; "use_osx_xattr_stubs"] &
      S[A"-dllib"; A"-losx_xattr_stubs"];
    dep ["ocaml"; "link"; "byte"; "program"; "use_osx_xattr_stubs"]
      ["lib/dllosx_xattr_stubs"-.-(!Options.ext_dll)];

    flag ["ocaml"; "link"; "native"; "program"; "use_osx_xattr_stubs"] &
      S[A"-cclib"; A"-losx_xattr_stubs"];
    dep ["ocaml"; "link"; "native"; "program"; "use_osx_xattr_stubs"]
      ["lib/libosx_xattr_stubs"-.-(!Options.ext_lib)];

    (* Linking Lwt tests *)
    flag ["ocaml"; "link"; "byte"; "program"; "use_osx_xattr_lwt_stubs"] &
      S[A"-dllib"; A"-losx_xattr_lwt_stubs"];
    dep ["ocaml"; "link"; "byte"; "program"; "use_osx_xattr_lwt_stubs"]
      ["lwt/dllosx_xattr_lwt_stubs"-.-(!Options.ext_dll)];

    flag ["ocaml"; "link"; "native"; "program"; "use_osx_xattr_lwt_stubs"] &
      S[A"-cclib"; A"-losx_xattr_lwt_stubs"];
    dep ["ocaml"; "link"; "native"; "program"; "use_osx_xattr_lwt_stubs"]
      ["lwt/libosx_xattr_lwt_stubs"-.-(!Options.ext_lib)];


  | _ -> ()
end;;
