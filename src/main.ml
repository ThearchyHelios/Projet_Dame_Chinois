type cordonnee = int * int * int;;
type dimension = int;;
let dimension = 3;;


(* Question 2 *)
(* SPÉCIFICATION : check_direction
 
 * SIGNATURE :     coordonnee -> string
 *
 * SÉMANTIQUE :    Renvoie la position d'un pion
 *
 * EXEMPLES :      check_direction (-1, 3, -2) = "Zone Centrale"
 *
 *)
let check_direction cordonnee =
  let dim = dimension in
  match cordonnee with
  | (x, y, z) when x > dim && y < 0 && z < 0 -> "Nord"
  | (x, y, z) when x > 0 && y > 0 && z < -dim -> "Nord_Est"
  | (x, y, z) when x < 0 && y > dim && z < 0 -> "Sud_Est"
  | (x, y, z) when x < -dim && y > 0 && z > 0 -> "Sud"
  | (x, y, z) when x < 0 && y < 0 && z > dim -> "Sud_Ouest"
  | (x, y, z) when x > 0 && y < -dim && z > 0 -> "Nord_Ouest"
  | (x, y, z) when x = 0 && y = 0 && z = 0 -> "Point Centre"
  | _ -> "Zone Centrale";;

check_direction (-1, 3, -2);;

let a:cordonnee = (-4, 2, 2) in
if check_direction a = "Point Centre" || check_direction a = "Zone Centrale" || check_direction a = "Nord" || check_direction a = "Sud" then
  "Question 2 Verifiee"
else
  "Question 2 Failed";;


  (*ceci est un test*)
(* Question 3 *)
let check_dimension dimension = match dimension with
| _ when dimension < 0 -> false
| _ when dimension > 0 -> true
| _ -> false;;

(* check_dimension (-4);; *)
let est_dans_etoile cordonnee dimension :bool =
  let x, y, z = cordonnee in
  if (x + y + z) != 0 then
    false
  else
    if (x <= dimension && x >= -dimension) && (y <= dimension && y >= -dimension) then true
    else
      if (y <= dimension && y >= -dimension) && (z <= dimension && z >= -dimension) then true
      else
        if (x <= dimension && x >= -dimension) && (z <= dimension && z >= -dimension) then true
        else
          false
        ;;

(* Verifier si la dimension est correct, si oui fonctionner le fonction <est_dans_etoile>, si non (ex: dimension = -3 ou dimension = 0) return false *)
if check_dimension dimension then
  let a = (0, 0, 0) in
  let b = dimension in
  est_dans_etoile a b
else
  false;;

est_dans_etoile (0, 0, 0) dimension;;


(* Question 4 *)
let tourner_case cordonnee (m:int) =
  let x, y, z = cordonnee in
  match m with
  | 1 -> (-y, -z, -x)
  | 2 -> (z, x, y)
  | 3 -> (-x, -y, -z)
  | 4 -> (-y, z, x)
  | 5 -> (-z, -x, -y)
  | 6 -> (x, y, z)
  | _ -> cordonnee;;

tourner_case (-1, 3, -2) 1;;


(* Question 5 *)

type vecteur = int * int * int;;

let translate cordonnee vecteur =
  let c1, c2, c3= cordonnee in
  let v1, v2, v3 = vecteur in
  (c1 + v1, c2 + v2, c3 + v3);;


(* Question 6 *)

let diff_case (c1:cordonnee) (c2:cordonnee) =
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  (x1 - x2, y1 - y2, z1 - z2);;


(* Question 7 *)

let diff_case_possitive (c1:cordonnee) (c2:cordonnee) =
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

diff_case (-1, 1, 0) (-1, 0, 1);;
diff_case_possitive (-1, 1, 0) (-1, 0, 1);;

let sont_cases_voisines (c1:cordonnee) (c2:cordonnee) =
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

sont_cases_voisines (0, -2, 4) (-1, -3, 4);;


(* Question 8 *)

let pair x =
  if x mod 2 = 0 then
    true
  else
    false;;

let calcul_pivot (c1:cordonnee) (c2:cordonnee) =
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

calcul_pivot (3,3, -6) (3, -5, 1);;


(* Question 9 *)

let vec_et_dict (c1:cordonnee) (c2:cordonnee) = 
  let d = diff_case_possitive c1 c2 in
  let x, y, z = d in
  let d0 = diff_case c1 c2 in
  let x0, y0, z0 = d0 in
  if x = 0 && y = 0  && z = 0 then
    None
  else
    if x = y && x != 0 && z = 0 then
      if x0 > 0 then
        Some((-1, 1, 0), x)
      else
        Some((1, -1, 0), x)
    else
      if x = z && x != 0 && y = 0 then
        if x0 > 0 then
          Some((-1, 0, 1), x)
        else
          Some((1, 0, -1), x)
      else
        if y = z && y != 0 && x = 0 then
          if y0 > 0 then
            Some((0, -1, 1), y)
          else
            Some((0, 1, -1), y)
        else
          None;;

vec_et_dict (-3, -2, 5) (-3, 5, -2);;