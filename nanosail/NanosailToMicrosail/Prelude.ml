module GC = GenerationContext


let pp_require_imports () : PP.document =
  let coq_imports =
    Auxlib.build_list (fun { add; _ } ->
        if Configuration.(get use_list_notations) then add "Lists.List";
        add "Classes.EquivDec";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(separate (twice hardline) [
      Coq.pp_require ~from:(Some "Coq"      ) ~import:true coq_imports;
      Coq.pp_require ~from:(Some "Katamaran") ~import:true [ "Semantics.Registers"; "Bitvector"; "Program" ];
      Coq.pp_require ~from:(Some "stdpp"    ) ~import:true [ "finite" ];
      Coq.pp_require ~from:(Some "Equations") ~import:true [ "Equations" ];
    ])


let pp_imports () : PP.document =
  let imports =
    Auxlib.build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        if Configuration.(get use_list_notations) then add "ListNotations";
      )
  in
  Coq.pp_imports imports


let pp_open_scopes () : PP.document =
  let scopes =
    Auxlib.build_list (fun { add; _ } ->
        add "string_scope";
        add "list_scope";
      )
  in
  Coq.pp_open_scopes scopes


let pp_definitions () : PP.document =
  let definitions = [
      Coq.pp_definition
        ~identifier:(PP.string "bitvector")
        ~parameters:[(PP.string "n", Some (PP.string "nat"))]
        (PP.string "bv n");
    ]
  in
  PP.(separate hardline definitions)


let pp_import_default_base () : PP.document =
  PP.string "Import DefaultBase."


let generate () : PP.document =
  PP.(separate (twice hardline) [
      pp_require_imports ();
      pp_imports ();
      pp_open_scopes ();
      pp_definitions ();
      pp_import_default_base ();
    ])


let generate_base_prelude () : PP.document GC.t =
  GC.return @@ PP.separate PP.hardline [
    Coq.pp_require ~from:(Some "Coq"      ) ~import:true  [ "Classes.EquivDec"; "Strings.String" ];
    Coq.pp_require ~from:(Some "stdpp"    ) ~import:false [ "finite" ];
    Coq.pp_require ~from:(Some "Equations") ~import:true  [ "Equations" ];
    Coq.pp_require                          ~import:true  [ "Katamaran.Base" ];
  ]
