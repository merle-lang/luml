open Base
open Stdio

module C = Configurator

let possibles : (string list) =
  ["lua"; "lua5.3"; "lua5.2"]

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let find_package (pc : C.Pkg_config.t) default =
  let rec inner possibles = 
    match possibles with
    | [] -> default
    | hd :: t ->
      (match C.Pkg_config.query pc ~package: hd with
       | Some pkg -> pkg
       | None -> inner t) 
  in
  inner possibles

let () =
  C.main ~name:"lua" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = []
      ; cflags = []
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
         find_package pc default
    in

    write_sexp "c_flags.sexp"         (sexp_of_list sexp_of_string ("-Wno-discarded-qualifiers" :: conf.cflags));
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
