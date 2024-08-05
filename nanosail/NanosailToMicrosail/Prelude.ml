module GC = GenerationContext
  

let generate_require_imports () =
  let coq_imports =
    Auxlib.build_list (fun { add; _ } ->
        if Configuration.(get use_list_notations) then add "Lists.List";
        add "Classes.EquivDec";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(separate (twice hardline) [
      Coq.require ~from:(Some "Coq"      ) ~import:true coq_imports;
      Coq.require ~from:(Some "Katamaran") ~import:true [ "Semantics.Registers"; "Bitvector"; "Program" ];
      Coq.require ~from:(Some "stdpp"    ) ~import:true [ "finite" ];
      Coq.require ~from:(Some "Equations") ~import:true [ "Equations" ];
    ])


let generate_imports () =
  let imports =
    Auxlib.build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        if Configuration.(get use_list_notations) then add "ListNotations";
      )
  in
  Coq.imports imports


let generate_open_scopes () =
  let scopes =
    Auxlib.build_list (fun { add; _ } ->
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


let generate_import_default_base () =
  PP.string "Import DefaultBase."


let generate () =
  PP.(separate (twice hardline) [
      generate_require_imports ();
      generate_imports ();
      generate_open_scopes ();
      generate_definitions ();
      generate_import_default_base ();
    ])


let generate_base_prelude () : PP.document GC.t =
  GC.return @@ PP.separate PP.hardline [
    Coq.require ~from:(Some "Coq"      ) ~import:true  [ "Classes.EquivDec"; "Strings.String" ];
    Coq.require ~from:(Some "stdpp"    ) ~import:false [ "finite" ];
    Coq.require ~from:(Some "Equations") ~import:true  [ "Equations" ];
    Coq.require                          ~import:true  [ "Katamaran.Base" ];
  ]
