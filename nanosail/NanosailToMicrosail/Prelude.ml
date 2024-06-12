open Auxlib


let generate_require_imports () =
  let coq_imports =
    build_list (fun { add; _ } ->
        if Configuration.(get use_list_notations) then add "Lists.List";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(separate (twice hardline) [
      Coq.require_imports "Coq" coq_imports;
      Coq.require_imports "Katamaran" [ "Semantics.Registers"; "Bitvector"; "Program" ];
      Coq.require_imports "stdpp" [ "finite" ];
      Coq.require_imports "Equations" [ "Equations" ];
    ])

let generate_imports () =
  let imports =
    build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        if Configuration.(get use_list_notations) then add "ListNotations";
      )
  in
  Coq.imports imports

let generate_open_scopes () =
  let scopes =
    build_list (fun { add; _ } ->
        add "string_scope";
        add "list_scope";
      )
  in
  Coq.open_scopes scopes

let generate_definitions () =
  let definitions = [
      Coq.definition
        ~identifier:(PP.string "bitvector")
        ~parameters:[(PP.string "n", Some (PP.string "nat"))]
        (PP.string "bv n");
    ]
  in
  PP.(separate hardline definitions)

let generate () =
  PP.(separate (twice hardline) [
      generate_require_imports ();
      generate_imports ();
      generate_open_scopes ();
      generate_definitions ();
    ])
