with import <nixpkgs> {}; 

with ocamlPackages_4_01_0; 
stdenv.mkDerivation {

    name = "ocs";
    CAML_LD_LIBRARY_PATH = "${ocaml_ssl}/lib/ocaml/4.01.0/site-lib/ssl:${ocaml_lwt}/lib/ocaml/4.01.0/site-lib/lwt:${ocamlnet}/lib/ocaml/4.01.0/site-lib/netsys:${ocamlnet}/lib/ocaml/4.01.0/site-lib/netstring:${ocaml_pcre}/lib/ocaml/4.01.0/site-lib/pcre:${cryptokit}/lib/ocaml/4.01.0/site-lib/cryptokit:${ocaml_sqlite3}/lib/ocaml/4.01.0/site-lib/sqlite3"; 	
    buildInputs = [ocaml findlib merlin eliom ocsigen_server ocsigen_deriving js_of_ocaml ocaml_ssl ocamlnet ocaml_pcre cryptokit ocaml_sqlite3 ocaml_batteries sqlite3];

}
