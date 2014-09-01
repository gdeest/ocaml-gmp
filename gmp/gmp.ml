open Ctypes
open Foreign

let foreign name typ = foreign name typ
  ~from:Dl.(dlopen ~filename:"libgmp.so.10.2.0" ~flags:[RTLD_NOW])

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
  let neg = foreign "__gmpz_neg" (t @-> t @-> returning void)

  let cdiv_q = foreign "__gmpz_cdiv_q" (t @-> t @-> t @-> returning void)
  let cdiv_r = foreign "__gmpz_cdiv_r" (t @-> t @-> t @-> returning void)
  let cdiv_qr = foreign "__gmpz_cdiv_qr" (t @-> t @-> t @-> t @-> returning void)

  let cmp = foreign "__gmpz_cmp" (t @-> t @-> returning int)
end

module Q_raw = struct
  type t = Types.q
  let t = Types.q

  let mpq_clear = foreign "__gmpq_clear" (t @-> returning void)
  let alloc () : t =
    let ptr = allocate_n ~count:1 Types.__mpq_struct in
    Gc.finalise mpq_clear ptr; ptr

  let canonicalize = foreign "__gmpq_canonicalize" (t @-> returning void)

  let init = foreign "__gmpq_init" (t @-> returning void)
  let build () =
    let v = alloc () in
    init v; v

  let set = foreign "__gmpq_set" (t @-> t @-> returning void)
  let set_z = foreign "__gmpq_set_z" (t @-> Types.z @-> returning void)
  let set_ui = foreign "__gmpq_set_ui" (t @-> ulong @-> ulong @-> returning void)
  let set_si = foreign "__gmpq_set_si" (t @-> long @-> long @-> returning void)
  let set_str = foreign "__gmpq_set_str" (t @-> string @-> int @-> returning void)
  let set_d = foreign "__gmpq_set_d" (t @-> double @-> returning void)
  let swap = foreign "__gmpq_swap" (t @-> t @-> returning void)

  let add = foreign "__gmpq_add" (t @-> t @-> t @-> returning void)
  let sub = foreign "__gmpq_sub" (t @-> t @-> t @-> returning void)
  let mul = foreign "__gmpq_mul" (t @-> t @-> t @-> returning void)
  let neg = foreign "__gmpq_neg" (t @-> t @-> returning void)
  let div = foreign "__gmpq_div" (t @-> t @-> t @-> returning void)

  let cmp = foreign "__gmpq_cmp" (t @-> t @-> returning int)

  let get_str = foreign "__gmpq_get_str" (ptr char @-> int @-> t @-> returning string)
end

module Z = struct
  type t = Types.z

  let zero = Z_raw.build ()
  let compare = Z_raw.cmp
  let (=) a b = compare a b = 0
  let (<>) a b = not (a = b)
  let (<) a b = compare a b < 0
  let (>) a b = compare a b > 0
  let (<=) a b = compare a b <= 0
  let (>=) a b = compare a b >= 0

  let of_int i =
    let i = Signed.Long.of_int i in
    let v = Z_raw.build () in
    Z_raw.set_si v i; v

  let of_int64 i =
    let i = Signed.Long.of_int64 i in
    let v = Z_raw.build () in
    Z_raw.set_si v i; v

  let of_q q =
    let v = Z_raw.build () in
    Z_raw.set_q v q; v

  let of_float f =
    let v = Z_raw.build () in
    Z_raw.set_d v f; v

  let of_string ?base:(b=10) str =
    let v = Z_raw.build () in
    Z_raw.set_str v str b; v

  let add v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.add ret v1 v2; ret

  let sub v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.sub ret v1 v2; ret

  let mul v1 v2 =
    let ret = Z_raw.build () in
    Z_raw.mul ret v1 v2; ret

  let neg v =
    let ret = Z_raw.build() in
    Z_raw.neg ret v; ret

  let div v1 v2 =
    if v2 = zero then raise Division_by_zero;
    let ret = Z_raw.build () in
    Z_raw.cdiv_q ret v1 v2; ret

  let modulo v1 v2 =
    if v2 = zero then raise Division_by_zero;
    let ret = Z_raw.build () in
    Z_raw.cdiv_r ret v1 v2; ret

  let div_with_remainder v1 v2 =
    if v2 = zero then raise Division_by_zero;
    let q, r = Z_raw.build (), Z_raw.build () in
    Z_raw.cdiv_qr q r v1 v2; (q,r)

  let to_string ?base:(b=10) v =
    Z_raw.get_str (from_voidp char null) b v

  let min a b = if a <= b then  a else b
  let max a b = if a >= b then  a else b
      
  let (+), (-), ( * ), (/), (mod), (~-) = add, sub, mul, div, modulo, neg
end

module Q = struct
  type t = Types.q

  let zero = Q_raw.build ()
  let compare = Q_raw.cmp
  let (=) a b = compare a b = 0
  let (<>) a b = not (a = b)
  let (<) a b = compare a b < 0
  let (>) a b = compare a b > 0
  let (<=) a b = compare a b <= 0
  let (>=) a b = compare a b >= 0

  let min a b = if a <= b then  a else b
  let max a b = if a >= b then  a else b

  let add v1 v2 =
    let ret = Q_raw.build () in
    Q_raw.add ret v1 v2; ret

  let sub v1 v2 =
    let ret = Q_raw.build () in
    Q_raw.sub ret v1 v2; ret

  let mul v1 v2 =
    let ret = Q_raw.build () in
    Q_raw.mul ret v1 v2; ret

  let div v1 v2 =
    if v2 = zero then raise Division_by_zero;
    let ret = Q_raw.build () in
    Q_raw.div ret v1 v2; ret

  let neg v =
    let ret = Q_raw.build() in
    Q_raw.neg ret v; ret

  let of_ints (n,d) =
    let v = Q_raw.build () in
    Q_raw.set_si v (Signed.Long.of_int n) (Signed.Long.of_int d);
    Q_raw.canonicalize v; v

  let of_ints64 (n,d) =
    let v = Q_raw.build () in
    Q_raw.set_si v (Signed.Long.of_int64 n) (Signed.Long.of_int64 d);
    Q_raw.canonicalize v; v

  let of_z z =
    let v = Q_raw.build () in
    Q_raw.set_z v z; v

  let of_float f =
    let v = Q_raw.build () in
    Q_raw.set_d v f; v

  let of_string ?base:(b=10) str =
    let v = Q_raw.build () in
    Q_raw.set_str v str b; v

  let to_string ?base:(b=10) v =
    Q_raw.get_str (from_voidp char null) b v

  let (+), (-), ( * ), (/), (~-) = add, sub, mul, div, neg
end
