type coordonnee = int * int * int  (*restreint au triplet tels (i,j,k) tels que i+j+k=0*);; (*type case au lieu de coordonnee*)
type dimension = int;;
let dimension = 3;;

(* Question 1 *)

(* SPÉCIFICATION : check_direction
 
 * SIGNATURE :     coordonnee -> string
 *
 * SÉMANTIQUE :    Renvoie la position d'un pion/case
 *
 * EXEMPLES :      check_direction (-1, 3, -2) = "Zone Centrale"
 *                 check_direction (-2, 4, -2) = "Sud_Est"
 *                 check_direction (3, -4, 1) = "Nord_Ouest"
 *)
let check_direction coordonnee =
  let dim = dimension in
  match coordonnee with
  | (i, j, k) when i > dim && j < 0 && k < 0 -> "Nord"
  | (i, j, k) when i > 0 && j > 0 && k < -dim -> "Nord_Est"
  | (i, j, k) when i < 0 && j > dim && k < 0 -> "Sud_Est"
  | (i, j, k) when i < -dim && j > 0 && k > 0 -> "Sud"
  | (i, j, k) when i < 0 && j < 0 && k > dim -> "Sud_Ouest"
  | (i, j, k) when i > 0 && j < -dim && k > 0 -> "Nord_Ouest"
  | (i, j, k) when i = 0 && j = 0 && k = 0 -> "Point Centre"
  | _ -> "Zone Centrale"
;;

assert(check_direction (-1, 3, -2) = "Zone Centrale");;

(* Question 2 *)

(*ici, on vérifie que l'on soit bien situé sur le losange Nord-Sud, que l'on a préalablement coupé en 4 sous-parties*)
let a:coordonnee = (-4, 2, 2) in
if check_direction a = "Point Centre" || check_direction a = "Zone Centrale" || check_direction a = "Nord" || check_direction a = "Sud" then
  "Question 2 Verifiee"
else
  "Question 2 Failed";;


(* SPÉCIFICATION : est_dans_losange
 * SIGNATURE :     coordonnee -> dimension -> bool
 *
 * SÉMANTIQUE :    Renvoie un booléen selon la position du pion par rapport au losange.
 *                 Ici, on vérifie que l'on soit bien situé sur le losange Nord-Sud, que l'on a préalablement coupé en 4 sous-parties
 *
 * EXEMPLES :
 *
 *   est_dans_losange (3, 3, -6) (3) = false
 *   est_dans_losange (-3, 3, 0) (3) = true
 *)
let est_dans_losange (c:coordonnee) (dim:dimension) : bool =
  if check_direction c = "Point Centre" || check_direction c = "Zone Centrale" || check_direction c = "Nord" || check_direction c = "Sud" then
    true
  else
    false
;;


(* Question 3 *)
(* SPÉCIFICATION : check_dimension
 
 * SIGNATURE :     dimension -> bool
 *
 * SÉMANTIQUE :    Vérifie que la dimension soit cohérente, donc positive (et pas dans une autre dimension...)
 *
 * EXEMPLES :      check_dimension (-4) = false
 *                 check_dimension (4) = true
 *)
let check_dimension dimension = match dimension with
| _ when dimension < 0 -> false
| _ when dimension > 0 -> true
| _ -> false;;

assert(check_dimension (-4) = false);;

(* SPÉCIFICATION : est_dans_etoile
 
 * SIGNATURE :     coordonnee -> dimension -> bool
 *
 * SÉMANTIQUE :    Vérifie qu'une case est bel et bien dans l'étoile. (Ni dans une planète, ni un tout autre astre...)
 *
 * EXEMPLES :      est_dans_etoile (2, 1, -3) (3) = true
 *                 est_dans_etoile (4, 6, -10) (3) = false
 *                 est_dans_etoile (4, 6, -10) (3) = false
 *)
let est_dans_etoile coordonnee dimension :bool =
  let i, j, k = coordonnee in
  if (i + j + k) != 0 then
    false
  else
    if (i <= dimension && i >= -dimension) && (j <= dimension && j >= -dimension) then true
    else
      if (j <= dimension && j >= -dimension) && (k <= dimension && k >= -dimension) then true
      else
        if (i <= dimension && i >= -dimension) && (k <= dimension && k >= -dimension) then true
        else
          false
        ;;

(* Verifier si la dimension est correct, si oui fonctionner le fonction <est_dans_etoile>, si non (ex: dimension = -3 ou dimension = 0) return false *)
(*
if check_dimension dimension then
  let a = (0, 0, 0) in
  let b = dimension in
  est_dans_etoile a b
else
  false;;

assert(est_dans_etoile (0, 0, 0) dimension = true);;
*)

(*
(* Question 4 *)
let tourner_case coordonnee (m:int) =
  let x, y, z = coordonnee in
  match m with
  | 1 -> (-y, -z, -x)
  | 2 -> (z, x, y)
  | 3 -> (-x, -y, -z)
  | 4 -> (-y, z, x)
  | 5 -> (-z, -x, -y)
  | 6 -> (x, y, z)
  | _ -> coordonnee;;

assert(tourner_case (-1, 3, -2) 1 = (-3, 2, 1));;


(* Question 5 *)

type vecteur = int * int * int;;

let translate coordonnee vecteur =
  let c1, c2, c3= coordonnee in
  let v1, v2, v3 = vecteur in
  (c1 + v1, c2 + v2, c3 + v3);;


(* Question 6 *)

let diff_case (c1:coordonnee) (c2:coordonnee) =
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  (x1 - x2, y1 - y2, z1 - z2);;


(* Question 7 *)

let diff_case_possitive (c1:coordonnee) (c2:coordonnee) =
  let d = diff_case c1 c2 in
  let x, y, z = d in
  if x < 0 then (-x, y, z)
  else
    if y < 0 then (x, -y, z)
    else
      if z < 0 then (x, y, -z)
      else
        (x, y, z)
      ;;

assert(diff_case (-1, 1, 0) (-1, 0, 1) = (0, 1, -1));;
assert(diff_case_possitive (-1, 1, 0) (-1, 0, 1) = (0, 1, 1));;

let sont_cases_voisines (c1:coordonnee) (c2:coordonnee) =
  let d = diff_case_possitive c1 c2 in
  let x, y, z = d in
  if x = 0 && y = 0 && z = 0 then
    false
  else
    if x = 1 && y = 1 && z = 0 then
      true
    else
      if x = 0 && y = 1 && z = 1 then
        true
      else
        if x = 1 && y = 0 && z = 1 then
          true
        else
          false
        ;;

assert(sont_cases_voisines (0, -2, 4) (-1, -3, 4) = true);;


(* Question 8 *)

let pair x =
  if x mod 2 = 0 then
    true
  else
    false;;

let calcul_pivot (c1:coordonnee) (c2:coordonnee) =
  let d = diff_case_possitive c1 c2 in
  let x, y, z = d in
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  if x = 0 && y = 0 && z = 0 then
    None
  else
    if x = y && x != 0 && z = 0 && pair x then
      Some((x1 + x2) / 2, (y1 + y2) / 2, z1)
    else
      if x = z && x != 0 && y = 0 && pair x then
        Some((x1 + x2) / 2, y1, (z1 + z2) / 2)
      else
        if y = z && y != 0 && x = 0 && pair y then
          Some(x1, (y1 + y2) / 2, (z1 + z2) / 2)
        else
          None;;

calcul_pivot (3,3, -6) (3, -5, 2);;
assert(calcul_pivot (3,3, -6) (3, -5, 2) = Some (3, -1, -2));;


(* Question 9 *)

let vec_et_dict2 (c1:coordonnee) (c2:coordonnee) =
  let d = diff_case_possitive c1 c2 in
  let x, y, z = d in
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  if x = 0 && y = 0  && z = 0 then
    None
  else
    if x = 0 then
      Some((0,(y2 - y1) / y, (z2 - z1) / z), y)
    else
      if y = 0 then
        Some(((x2 - x1) / x, 0, (z2 - z1) / z), x)
    else
      if z = 0 then
        Some(((x2 - x1) / x, (y2 - y1) / y, 0), x)
    else
      None;;

<<<<<<< Updated upstream
assert(vec_et_dict2 (-3, -2, 5) (-3, 5, -2) = Some ((0, 1, -1), 7));; *)

(* SUUUUUUUUUUUUUUUUUUUUUUUUUUUuu*)
=======
assert(vec_et_dict2 (-3, -2, 5) (-3, 5, -2) = Some ((0, 1, -1), 7));;
*)
>>>>>>> Stashed changes
