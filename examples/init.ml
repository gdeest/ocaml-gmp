open Gmp

let _ =
  let v1 = Z.of_int 10 and
      v2 = Z.of_int 321 and
      v3 = Z.of_int 13 in
  let ret = Z.(v1 * v2 + v3) in
  let ret2 = Z.(ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret * ret) in
  print_endline (Z.to_string ~base:10 ret);
  print_endline (Z.to_string ~base:16 ret);
  print_endline (Z.to_string ~base:10 ret2);
  print_endline Z.(to_string @@ ret2 / ret);
  print_endline Z.(to_string @@ ret2 mod ret);
  print_endline Z.(to_string ~base:10 @@ ret2 / ret2)
    

type 'a matrix = 
  { dims : int * int;
    data : 'a array }

(* let mk_matrix (n,m) = *)
(*   { dims = (n,m); *)
(*     data = Array.init n*m (fun _ -> Z.build ()) *)
(*   } *)
    

let init_many n =
  Array.init n Z.of_int

let _ = 
  let arr = init_many 1000 in
  Array.iter (fun i -> print_endline @@ Z.to_string i) arr

let _ =
  let v1 = Q.of_ints (3, 6) in
  print_endline @@ Q.to_string v1

let _ =
  print_endline Z.(to_string @@ zero / zero)
