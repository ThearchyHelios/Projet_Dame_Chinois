type case = int * int * int  (*restreint au triplet tels (i,j,k) tels que i+j+k=0*);; (*type case au lieu de case*)
type dimension = int;;
let dimension = 3;;

(* Question 1 *)

(* SPÉCIFICATION : check_direction
 
 * SIGNATURE :     case -> string
 *
 * SÉMANTIQUE :    Renvoie la position d'un pion/case
 *
 * EXEMPLES :      check_direction (-1, 3, -2) = "Zone Centrale"
 *                 check_direction (-2, 4, -2) = "Sud_Est"
 *                 check_direction (3, -4, 1) = "Nord_Ouest"
 *)
let check_direction case =
  let dim = dimension in
  match case with
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
let a:case = (-4, 2, 2) in
if check_direction a = "Point Centre" || check_direction a = "Zone Centrale" || check_direction a = "Nord" || check_direction a = "Sud" then
  "Question 2 Verifiee"
else
  "Question 2 Failed";;


(* SPÉCIFICATION : est_dans_losange
 * SIGNATURE :     case -> dimension -> bool
 *
 * SÉMANTIQUE :    Renvoie un booléen selon la position du pion par rapport au losange.
 *                 Ici, on vérifie que l'on soit bien situé sur le losange Nord-Sud, que l'on a préalablement coupé en 4 sous-parties
 *
 * EXEMPLES :
 *
 *   est_dans_losange (3, 3, -6) (3) = false
 *   est_dans_losange (-3, 3, 0) (3) = true
 *)
let est_dans_losange (c:case) (dim:dimension) : bool =
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
let check_dimension (dim:dimension) : bool = 
  match dim with
| _ when dim < 0 -> false
| _ when dim > 0 -> true
| _ -> false;;

assert(check_dimension (-4) = false);;

(* SPÉCIFICATION : est_dans_etoile
 
 * SIGNATURE :     case -> dimension -> bool
 *
 * SÉMANTIQUE :    Vérifie qu'une case est bel et bien dans l'étoile. (Ni dans une planète, ni un tout autre astre...)
 *
 * EXEMPLES :      est_dans_etoile (2, 1, -3) (3) = true
 *                 est_dans_etoile (4, 6, -10) (3) = false
 *                 est_dans_etoile (4, 6, -10) (3) = false
 *)
let est_dans_etoile (c:case) (dim:dimension) :bool =
  let i, j, k = c in
  if (i + j + k) != 0 then
    false
  else
    if (i <= dim && i >= -dim) && (j <= dim && j >= -dim) then true
    else
      if (j <= dim && j >= -dim) && (k <= dim && k >= -dim) then true
      else
        if (i <= dim && i >= -dim) && (k <= dim && k >= -dim) then true
        else
          false
        ;;

(* Verifier si la dimension est correcte, si oui utiliser la fonction <est_dans_etoile>, sinon (ex: dimension = -3 ou dimension = 0) return false *)
(*
if check_dimension dimension then
  let a = (0, 0, 0) in
  let b = dimension in
  est_dans_etoile a b
else
  false;;

assert(est_dans_etoile (0, 0, 0) dimension = true);;
*)

(* Question 4 *)

(* SPÉCIFICATION : tourner_case
 
 * SIGNATURE :     case -> int -> case
 *
 * SÉMANTIQUE :    Permet de garder le meme emplacement des pions selon m-sixieme de tour de plateau dans le sens antihoraire
 *
 * EXEMPLES :      tourner_case (-4, 1, 3) (3) = (-1, -3, 4)
 *                 tourner_case (4, -3, -1) (3) = (-4, 3, 1)
 *)

let tourner_case (c:case) (m:int) : case =
  let x, y, z = c in
  match m with
  | 1 -> (-y, -z, -x)
  | 2 -> (z, x, y)
  | 3 -> (-x, -y, -z)
  | 4 -> (-y, z, x)
  | 5 -> (-z, -x, -y)
  | 6 -> (x, y, z)
  | _ -> c ;; (* pour remplir tous les cas possible avec le match with, mais une valeur autre 1,2,3,4,5 ou 6 ne sera jamais atteinte *)


(* assert(tourner_case (-1, 3, -2) 1 = (-3, 2, 1)) *);;


(* Question 5 *)

type vecteur = int * int * int;;

(* SPÉCIFICATION : translate
 
 * SIGNATURE :     case -> vecteur -> case
 *
 * SÉMANTIQUE :    Effectue une translation d'une case par un vecteur
 *
 * EXEMPLES :      translate (3, 0, -3) (-1, 2, -1) = (2, 2, -4)
 *                 translate (1, -4, 3) (2, -2, 0) = (3, -6, 3)
 *)

let translate (c:case) (v:vecteur) : case =
  let c1, c2, c3= c in
  let v1, v2, v3 = v in
  (c1 + v1, c2 + v2, c3 + v3);;


(* Question 6 *)

(* SPÉCIFICATION : diff_case
 
 * SIGNATURE :     case -> case -> vecteur
 *
 * SÉMANTIQUE :    Calcule la différence de chacune des coordonnées puis renvoie un vecteur de translation
 *
 * EXEMPLES :      diff_case (3, 1, -4) (5, -1, -4) = = (-2, 2, 0)
 *                 diff_case (2, 3, -5) (0, -1, 1) = (2, 4, -6)
 *)

let diff_case (c1:case) (c2:case) : vecteur =
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  (x1 - x2, y1 - y2, z1 - z2);;


(* Question 7 *)

(* SPÉCIFICATION : diff_case_positive
 
 * SIGNATURE :     case -> case -> case
 *
 * SÉMANTIQUE :    apres avoir calculé un vecteur de translation avec la fonction diif_case, transforme tout point négatif en positif et renvoie un nouveau vecteur
 *
 * EXEMPLES :      diff_case_positive (-1, 1, 0) (-1, 0, 1) = (0, 1, 1)
 *                 diff_case_positive (-1, 2, -3) (1, -4, 2) = (2, 6, -5)
 *)
 
let diff_case_positive (c1:case) (c2:case) : vecteur =
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
assert(diff_case_positive (-1, 1, 0) (-1, 0, 1) = (0, 1, 1));;


(* SPÉCIFICATION : sont_cases_voisines
 
 * SIGNATURE :     case -> case -> bool
 *
 * SÉMANTIQUE :    apres avoir rendu un vecteur de translation positif avec diff_case_positive, renvoie True ssi deux points du vecteur sont égaux à 1
 *
 * EXEMPLES :      sont_cases_voisines (0, -2, 4) (-1, -3, 4) = true
 *                 sont_cases_voisines (-2, -2, 4) (-5, 3, 2) = false
 *)


let sont_cases_voisines (c1:case) (c2:case) : bool =
  let d = diff_case_positive c1 c2 in
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

(* SPÉCIFICATION : calcul_pivot
 
 * SIGNATURE :     case -> case -> case option
 *
 * SÉMANTIQUE :    renvoie les coordonnées de la case pivot, ce qui permettra au pion de se déplacer correctement
 *
 * EXEMPLES :      calcul_pivot (3,3, -6) (3, -5, 2) = Some (3, -1, -2) 
 *                 calcul_pivot (2, 1, -3) (4, -5, 1) = None
 *)


let calcul_pivot (c1:case) (c2:case) =
  let d = diff_case_positive c1 c2 in
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

(* SPÉCIFICATION : vect_et_dist2
 
 * SIGNATURE :     case -> case -> vecteur*int
 *
 * SÉMANTIQUE :    renvoie le vecteur de déplacement unitaire et la distance entre 2 cases
 *
 * EXEMPLES :      vec_et_dist (0, 2, -2) (0,0,0) = Some ((0, -1, 1), 2) 
 *                 vec_et_dist (3, 2, -5) (1, 2, 3) = Some ((-1, 0, 1), 2)
 *)

let vec_et_dist (c1:case) (c2:case) =
  let d = diff_case_positive c1 c2 in
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

assert(vec_et_dist (-3, -2, 5) (-3, 5, -2) = Some ((0, 1, -1), 7));;


(* Question 10*)

let tourner_list liste =
  let first = List.hd liste in
  let last = List.tl liste in
  last @ [first];;

tourner_list [1;2;3];;

let der_liste liste =
  let length = List.length liste in
  List.nth liste (length-1) ;;

let rec derniere_element_list list =
  match list with
  | [] -> failwith "liste vide"
  | [x] -> x
  | pr::fin -> derniere_element_list fin;;

derniere_element_list [1;2;3;5];;

der_liste [1;2;3;5];;
assert(der_liste [1;2;3] = 3);;

derniere_element_list["Vert"; "Rouge"; "Jaune"; "Bleu"];;


(* Question 11 *)

let remplir_segment (a:int) (case:case) =
  let x, y, z = case in
  let rec remplir_segment_rec return_list a =
    match a with
    | 0 -> return_list
    | _ -> remplir_segment_rec (List.cons (x, y + a - 1, z - a + 1) return_list) (a - 1) in
  remplir_segment_rec [] a;;

remplir_segment 1 (0, 0, 0);;
remplir_segment 3 (-4, 1, 3);;

(* Question 12 *)

est_dans_etoile (-3, 4, -1) 3;;


let remplir_triangle_bas (a:int) (case:case) =
  let x, y, z = case in
  match a with
  | 0 -> [(x, y, z)]
  | _ -> let rec remplir_triangle_bas_rec return_list_b b =
      match b with
      | 0 -> return_list_b
      | _ -> let rec remplir_triangle_bas_rec_bis return_list_c c =
                match c with
                | 0 -> return_list_c
                | _ -> remplir_triangle_bas_rec_bis (List.cons (x + c - 1, y + b - c, z -b +c) return_list_c) (c - 1) in
          remplir_triangle_bas_rec ((remplir_triangle_bas_rec_bis return_list_b (b))) (b - 1) in
    remplir_triangle_bas_rec [] (a);;


remplir_triangle_bas 3 (-3, 4, -1);;