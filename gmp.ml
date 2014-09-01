open Ctypes
open Foreign

module Types = struct
  (* Integers *)
  type __mpz_struct
  let __mpz_struct : __mpz_struct structure typ = structure "__mpz_struct"

  (* These fields are just there to set correct struct size for allocation. *)
  let _ = field __mpz_struct "_mp_alloc" int
  let _ = field __mpz_struct "_mp_size" int
  let _ = field __mpz_struct "_mp_d" (ptr void)
  let _ = seal __mpz_struct

  type z = __mpz_struct structure ptr
  let z = ptr __mpz_struct

  (* Rationals *)
  type __mpq_struct
  let __mpq_struct : __mpq_struct structure typ = structure "__mpq_struct"
  let _ = field __mpq_struct "_mp_num" __mpz_struct
  let _ = field __mpq_struct "_mp_den" __mpz_struct
  let _ = seal __mpq_struct

  type q = __mpq_struct structure ptr
  let q = ptr __mpq_struct
end

module Z_raw = struct
  type t = Types.z
  let t = Types.z


  let mpz_clear = foreign "__gmpz_clear" (t @-> returning void)

  let alloc () : t =
    let ptr = allocate_n ~count:1 Types.__mpz_struct in
    Gc.finalise mpz_clear ptr; ptr

  let init = foreign "__gmpz_init" (t @-> returning void) 

  let build () = 
    let v = alloc () in
    init v; v

  let set = foreign "__gmpz_set" (t @-> t @-> returning void)
  let set_ui = foreign "__gmpz_set_ui" (t @-> ulong @-> returning void)
  let set_si = foreign "__gmpz_set_si" (t @-> long @-> returning void)
  let set_d = foreign "__gmpz_set_d" (t @-> double @-> returning void)
  let set_q = foreign "__gmpz_set_q" (t @-> Types.q @-> returning void)
  (* TODO: mpz_set_f *)
  let set_str = foreign "__gmpz_set_f" (t @-> string @-> int @-> returning void)
  let swap = foreign "__gmpz_swap" (t @-> t @-> returning void)

  (* Format as string *)
  let get_str = foreign "__gmpz_get_str" (ptr char @-> int @-> t @-> returning string)
  let to_str = get_str (from_voidp char null)

  let add = foreign "__gmpz_add" (t @-> t @-> t @-> returning void)
  let sub = foreign "__gmpz_sub" (t @-> t @-> t @-> returning void)
  let mul = foreign "__gmpz_mul" (t @-> t @-> t @-> returning void)
end

module Z = struct
  let zero = Z_raw.build ()

  let of_int i =
    let i = Signed.Long.of_int i in
    let v = Z_raw.build () in
    Z_raw.set_si v i; v

  let add v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.add ret v1 v2; ret

  let sub v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.sub ret v1 v2; ret

  let mul v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.mul ret v1 v2; ret

  let (+), (-), ( * ) = add, sub, mul
end
