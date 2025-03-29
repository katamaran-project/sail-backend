open ExtBase


class virtual location =
  object (_self)
    method virtual to_string : string
  end


class string_location (index : int) =
  object (_self)
    inherit location

    method to_string =
      Int.to_string index
  end
