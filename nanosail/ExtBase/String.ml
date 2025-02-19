include Base.String


let rec repeat (string : string) (n : int) : string =
  if
    Base.Int.equal 0 n
  then
    ""
  else
    string ^ (repeat string (n-1))


let indent_strings (indentation : int) (strings : string list) : string list =
  let indent string =
    repeat " " indentation ^ string
  in
  List.map ~f:indent strings
