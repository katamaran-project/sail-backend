open ExtBase


(* todo find better place *)
let convert_bits_to_z (bits : bool list) : Z.t =
  let two   = Z.of_int 2
  and ( + ) = Z.add
  and ( * ) = Z.mul
  in
  List.fold bits ~init:Z.zero ~f:(fun acc bit -> acc * two + (if bit then Z.one else Z.zero))
