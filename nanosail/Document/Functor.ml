open ExtBase


module type ANNOTATION = sig
  type t

  val empty    : t
  val is_empty : t -> bool
  val combine  : t -> t -> t
  val to_html  : t -> Html.t
end

module type DOCUMENT = sig
  type t
  type annotation

  val is_empty       : t -> bool
  val is_single_line : t -> bool

  val measure        : t -> int * int
  val decorate       : AnsiColor.Decoration.t list -> t -> t
  val undecorate     : t -> t

  val to_string      : t -> string
  val to_html        : t -> Html.t

  val empty          : t
  val string         : string -> t
  val horizontal     : t list -> t
  val vertical       : t list -> t
  val annotate       : annotation -> t -> t
end


module Make(Annotation : ANNOTATION) : (DOCUMENT with type annotation = Annotation.t) = struct
  (*
     Should never be created directly by client.
     Factory functions are to be used instead.
  *)
  type t =
    | Empty
    | String     of string
    | Horizontal of t * t
    | Vertical   of t * t
    | Annotated  of t * Annotation.t
    | Decorated  of t * AnsiColor.Decoration.t list

  type annotation = Annotation.t


  let rec is_empty (document : t) : bool =
    (* relies on the fact that document values are always created using the factory functions and therefore normalized *)
    match document with
     | Empty              -> true
     | String _           -> false
     | Horizontal (_, _)  -> false
     | Vertical (_, _)    -> false
     | Annotated (doc, _) -> is_empty doc
     | Decorated (doc, _) -> is_empty doc


  let rec is_single_line (document : t) : bool =
    match document with
    | Empty               -> true
    | String _            -> true
    | Horizontal (d1, d2) -> is_single_line d1 && is_single_line d2
    | Vertical (_, _)     -> false
    | Annotated (doc, _)  -> is_single_line doc
    | Decorated (doc, _)  -> is_single_line doc


  type annotated_string =
    | AnnotatedString of { string : string; annotation : Annotation.t; undecorated_length : int }
    | Concatenation of annotated_string * annotated_string


  let rec length (annotated_string : annotated_string) : int =
    match annotated_string with
    | AnnotatedString { undecorated_length; _ } -> undecorated_length
    | Concatenation (s1, s2)                    -> length s1 + length s2


  let to_annotated_strings (document : t) : annotated_string list =
    let rec to_annotated_strings
        (accumulated_annotation : Annotation.t               )
        (current_decoration     : AnsiColor.Decoration.t list)
        (document               : t                          ) : annotated_string list
      =
      match document with
      | Empty -> []

      | String string -> begin
          [
            AnnotatedString {
              string;
              annotation=accumulated_annotation;
              undecorated_length = String.length string
            }
          ]
        end

      | Horizontal (left, right) -> begin
          let left'  = to_annotated_strings accumulated_annotation current_decoration left
          and right' = to_annotated_strings accumulated_annotation current_decoration right
          in
          match List.split_last left' with
          | Some (upper_left', last_left') -> begin
              match right' with
              | [] -> failwith "error"
              | first_right' :: bottom_right' -> begin
                  let indentation =
                    AnnotatedString {
                      string             = String.repeat " " (length last_left');
                      annotation         = Annotation.empty;
                      undecorated_length = length last_left';
                    }
                  in
                  List.concat [
                    upper_left';
                    [Concatenation (last_left', first_right')];
                    List.map ~f:(fun s -> Concatenation (indentation, s)) bottom_right'
                  ]
                end
            end
          | None -> failwith "TODO"
        end

      | Vertical (top, bottom) -> begin
          List.concat [
            to_annotated_strings accumulated_annotation current_decoration top;
            to_annotated_strings accumulated_annotation current_decoration bottom
          ]
        end

      | Decorated (subdocument, decorations) -> begin
          let subresults =
            to_annotated_strings accumulated_annotation decorations subdocument
          in
          let activate_decoration =
            AnsiColor.to_escape_sequence decorations
          and deactivate_decoration =
            AnsiColor.to_escape_sequence current_decoration
          in
          let decorate_string (string : string) : string
            =
            String.concat [
              activate_decoration;
              string;
              deactivate_decoration
            ]
          in
          let rec decorate_annotated_string (annotated_string : annotated_string) : annotated_string =
            match annotated_string with
             | AnnotatedString { string; annotation; undecorated_length } -> AnnotatedString { string = decorate_string string; annotation; undecorated_length }
             | Concatenation (child_1, child_2)                           -> Concatenation (decorate_annotated_string child_1, decorate_annotated_string child_2)
          in
          List.map ~f:decorate_annotated_string subresults
        end

      | Annotated (doc, annotation) -> begin
          to_annotated_strings
            (Annotation.combine accumulated_annotation annotation)
            current_decoration
            doc
        end
    in
    to_annotated_strings Annotation.empty [] document


  let rec strip_annotation (s : annotated_string) : string =
    match s with
    | AnnotatedString {string; _} -> string
    | Concatenation (s1, s2)      -> String.concat ~sep:"" [strip_annotation s1; strip_annotation s2]


  let to_strings (document : t) : string list =
    List.map ~f:strip_annotation @@ to_annotated_strings document


  let measure_width (document : t) : int =
    let strings = to_strings document
    in
    Option.value ~default:0 begin
      List.max_elt ~compare:Int.compare @@ List.map ~f:String.length strings
    end


  let measure_height (document : t) : int =
    let strings = to_strings document
    in
    List.length strings


  (* todo improve implementation *)
  let measure (document : t) : int * int =
    (measure_width document, measure_height document)


  let decorate
      (decorations : AnsiColor.Decoration.t list)
      (document    : t                          ) : t
    =
    if
      is_empty document
    then
      Empty
    else
      Decorated (document, decorations)

  let rec undecorate (document : t) : t =
    match (document : t) with
    | Empty                               -> document
    | String _                            -> document
    | Horizontal (document_1, document_2) -> Horizontal (undecorate document_1, undecorate document_2)
    | Vertical (document_1, document_2)   -> Vertical (undecorate document_1, undecorate document_2)
    | Annotated (document, annotation)    -> Annotated (undecorate document, annotation)
    | Decorated (document, _)             -> document


  let to_string (document : t) : string =
    String.concat ~sep:"\n" @@ to_strings document


  let to_html (document : t) : Html.t =
    let rec html_of_annotated_string (s : annotated_string) : Html.t =
      match s with
      | AnnotatedString {string; annotation; undecorated_length = _} -> begin
          if
            Annotation.is_empty annotation
          then
            Html.escape_string string
          else
            Html.span
              ~class_name:"tooltipped"
              begin
                Html.concat [
                    Html.string string;
                    Html.div ~class_name:"tooltip" begin
                        Annotation.to_html annotation
                      end
                  ]
              end
        end
      | Concatenation (s1, s2) -> begin
          Html.concat [
            html_of_annotated_string s1;
            html_of_annotated_string s2
          ]
        end
    in
    let annotated_strings =
      to_annotated_strings document
    in
    let html_lines =
      List.map
        ~f:(fun s -> Html.concat [ html_of_annotated_string s; Html.break ])
        annotated_strings
    in
    Html.concat html_lines


  let empty      = Empty


  let rec horizontal (documents : t list) : t =
    let group d1 d2 =
      match d1, d2 with
      | Empty, _ -> d2
      | _, Empty -> d1
      | _, _     -> Horizontal (d1, d2)
    in
    match documents with
    | []       -> empty
    | [d]      -> d
    | [d1; d2] -> group d1 d2
    | d::ds    -> group d @@ horizontal ds


  let rec vertical (documents : t list) : t =
    let group d1 d2 =
      match d1, d2 with
      | Empty, _ -> d2
      | _, Empty -> d1
      | _, _     -> Vertical (d1, d2)
    in
    match documents with
    | []       -> String ""
    | [d]      -> d
    | [d1; d2] -> group d1 d2
    | d::ds    -> group d @@ vertical ds


  (*
     If given a multiline string, splits it up in lines and arranges them vertically.
  *)
  let string (string : string) : t =
    match String.split_lines string with
    | []    -> Empty
    | [ s ] -> String s
    | lines -> vertical @@ List.map ~f:(fun s -> String s) lines


  let annotate
      (annotation : Annotation.t)
      (document   : t           ) : t
    =
    Annotated (document, annotation)
end
