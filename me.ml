type case = int * int * int  (*restreint au triplet tels (i,j,k) tels que i+j+k=0*);; (*type case au lieu de case*)
type dimension = int;;
let dimension = 3;;

let nb_jours = 6;;

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


let calcul_pivot (c1:case) (c2:case) : case =
  let d = diff_case_positive c1 c2 in
  let x, y, z = d in
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  if x = 0 && y = 0 && z = 0 then
    failwith("Il n'y a pas de reponses")
  else
    if x = y && x != 0 && z = 0 && pair x then
      (x1 + x2) / 2, (y1 + y2) / 2, z1
    else
      if x = z && x != 0 && y = 0 && pair x then
        (x1 + x2) / 2, y1, (z1 + z2) / 2
      else
        if y = z && y != 0 && x = 0 && pair y then
          x1, (y1 + y2) / 2, (z1 + z2) / 2
        else
          failwith("Il n'y a pas de reponses");;

calcul_pivot (3,3, -6) (3, -5, 2);;
assert(calcul_pivot (3,3, -6) (3, -5, 2) = (3, -1, -2));;


(* Question 9 *)

(* SPÉCIFICATION : vect_et_dist2
 
 * SIGNATURE :     case -> case -> vecteur*int
 *
 * SÉMANTIQUE :    renvoie le vecteur de déplacement unitaire et la distance entre 2 cases
 *
 * EXEMPLES :      vec_et_dist (0, 2, -2) (0,0,0) = Some ((0, -1, 1), 2) 
 *                 vec_et_dist (3, 2, -5) (1, 2, 3) = Some ((-1, 0, 1), 2)
 *)

let vec_et_dist (c1:case) (c2:case) : case*dimension =
  let d = diff_case_positive c1 c2 in
  let x, y, z = d in
  let x1, y1, z1 = c1 in
  let x2, y2, z2 = c2 in
  if x = 0 && y = 0  && z = 0 then
    failwith("Il n'y a pas de reponse")
  else
    if x = 0 then
      (0,(y2 - y1) / y, (z2 - z1) / z), y
    else
      if y = 0 then
        ((x2 - x1) / x, 0, (z2 - z1) / z), x
    else
      if z = 0 then
        ((x2 - x1) / x, (y2 - y1) / y, 0), x
    else
      failwith("Il n'y a pas de reponse");;

assert(vec_et_dist (-3, -2, 5) (-3, 5, -2) = ((0, 1, -1), 7));;


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

let rec remplir_segment_refaire (a:int) (case:case) =
  let x, y, z = case in
  match a with
  | 0 -> []
  | _ -> List.cons (x, y + a - 1, z - a + 1) (remplir_segment_refaire (a - 1) (case));;

remplir_segment_refaire 3 (-4, 1, 3);;


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
                | _ -> remplir_triangle_bas_rec_bis (List.cons (x + c - 1, y + b - c, - x - y - b + 1) return_list_c) (c - 1) in
          remplir_triangle_bas_rec ((remplir_triangle_bas_rec_bis return_list_b (b))) (b - 1) in
    remplir_triangle_bas_rec [] (a);;


remplir_triangle_bas 3 (-3, 4, -1);;

(* Question 13 *)
(* inverser les deux fonctions, triangle_bas fais le haut, triangle_haut fais le bas*)
let remplir_triangle_haut (a:int) (case:case) =
  let x, y, z = case in
  match a with
  | 0 -> [(x, y, z)]
  | _ -> let rec remplir_triangle_haut_rec return_list_b b =
      match b with
      | 0 -> return_list_b
      | _ -> let rec remplir_triangle_haut_rec_bis return_list_c c =
                match c with
                | 0 -> return_list_c
                | _ -> remplir_triangle_haut_rec_bis (List.cons (x - c + 1, - x - z + b - 1 , z - b + c) return_list_c) (c - 1) in
          remplir_triangle_haut_rec ((remplir_triangle_haut_rec_bis return_list_b (b))) (b - 1) in
    remplir_triangle_haut_rec [] (a);;

remplir_triangle_haut 3 (3, 1, -4);;


(* Question 14 *)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron | Code of string (*une chaine restreinte a 3 chr *) | Libre;;

type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;;

(* JUSTE *)
let colorie (color:couleur) (list_case:case list) : case_coloree list =
  let rec list_case_coloree return_list list_case color =
    match list_case with
    | [] -> return_list
    | pr :: fin -> let couple = [(pr, color)]@return_list in
      list_case_coloree couple fin color in
  list_case_coloree [((List.hd list_case),color)] (List.tl list_case) color;;

(* let colorie_2 (color:couleur) (cs:case list) : case_coloree list =

;; *)


(* Question 15 *)
(* JUSTE *)
(*La liste des joueurs - couleur - n'est pas tourner*)
let tourner_config (config:configuration) : configuration =
  let case_coloree, couleur_list, dimension = config in
  let tour_tourner = 6 / nb_jours in
  let rec tourner_case_list return_list case_coloree =
    match case_coloree with
    | [] -> return_list
    | pr :: fin -> let case, couleur = pr in
      tourner_case_list (List.cons (tourner_case case tour_tourner, couleur) return_list) fin in
  tourner_case_list [] case_coloree, tourner_list couleur_list, dimension;;

tourner_config ( [(1,2,-3), Bleu], [Bleu; Rouge; Vert], 3);;  

(*Question 16*)

type liste_joueur = couleur list ;;

(* JUSTE *)
let remplir_init_2 (a:liste_joueur) (dim:dimension) : configuration =
  let longueur = List.length a in
    match longueur with
    | 1 -> let triangle_sud = remplir_triangle_bas dim (-4,1,3) in
      let case_color_sud = colorie (List.nth a 0) triangle_sud in
      case_color_sud, a, dim
    | 2 -> let triangle_sud = remplir_triangle_bas dim (-4,1,3) in
      let case_color_sud = colorie (List.nth a 0) triangle_sud in
      let triangle_nord = remplir_triangle_haut dim (4,-3,-1) in
      let case_color_nord = colorie (List.nth a 1) triangle_nord in
      case_color_sud@case_color_nord, a, dim
    | 3 -> let triangle_sud = remplir_triangle_bas dim (-4,1,3) in
      let case_color_sud = colorie (List.nth a 0) triangle_sud in
      let triangle_nord_ouest = remplir_triangle_bas dim (3,-6,3) in
      let case_color_nord_ouest = colorie (List.nth a 1) triangle_nord_ouest in
      let triangle_nord_est = remplir_triangle_bas dim (3,1,-4) in
      let case_color_nord_est = colorie (List.nth a 2) triangle_nord_est in
      case_color_sud@case_color_nord_ouest@case_color_nord_est, a, dim
    | 6 -> let triangle_sud = remplir_triangle_bas dim (-4,1,3) in
      let case_color_sud = colorie (List.nth a 0) triangle_sud in
      let triangle_sud_ouest = remplir_triangle_haut dim (-3,-3,6) in
      let case_color_sud_ouest = colorie (List.nth a 1) triangle_sud_ouest in
      let triangle_nord_ouest = remplir_triangle_bas dim (3,-6,3) in
      let case_color_nord_ouest = colorie (List.nth a 2) triangle_nord_ouest in
      let triangle_nord = remplir_triangle_haut dim (4,-3,-1) in
      let case_color_nord = colorie (List.nth a 3) triangle_nord in
      let triangle_nord_est = remplir_triangle_bas dim (3,1,-4) in
      let case_color_nord_est = colorie (List.nth a 4) triangle_nord_est in
      let triangle_sud_est = remplir_triangle_haut dim (-3,4,-1) in
      let case_color_sud_est = colorie (List.nth a 5) triangle_sud_est in
      case_color_sud@case_color_sud_ouest@case_color_nord_ouest@case_color_nord@case_color_nord_est@case_color_sud_est, a, dim
    | _ -> failwith("Nombre de joueurs invalides");;
(* remplir_init ["Kil";"Man"] 3 *)


(*Question 17*)
(* JUSTE *)
let associe (a:case) (config:configuration) : couleur =
  let list_case, list_couleur, dim = config in
  let rec case_in_list list_case =
    match list_case with
    | [] -> Libre
    | pr::fin -> let case_1, couleur = pr in
      if case_1 = a
         then couleur
      else
        case_in_list fin in
  case_in_list list_case ;;

(*Question 18*)

(* JUSTE *)
let supprime_dans_config (conf:configuration) (c:case) : configuration =
  let list_case, list_couleur, dim = conf in
  let rec suppr list_case =
    match list_case with
    | [] -> []
    | pr::fin -> let case, couleur = pr in
      if case = c then
        suppr fin
      else
        [pr]@(suppr fin) in
  (suppr list_case), list_couleur, dim ;;


(*Question 22*)

let est_libre_seg (c1:case) (c2:case) (conf:configuration) : bool =
  let vect_unit, distance = vec_et_dist c1 c2 in
  let vx, vy, vz = vect_unit in
  let rec seg_libre (c1:case) (conf:configuration) (distance:int) : bool =
    match distance with
    | -1 -> false
    | 0 -> true
    | _ -> if (associe c1 conf) = Libre then
        let distance1 = distance - 1 in
        let cx, cy, cz = c1 in
        let cx2 = cx + vx in 
        let cy2 = cy + vy in 
        let cz2 = cz + vz in
        seg_libre (cx2,cy2,cz2) conf distance1
      else
        let distance1 = -1 in
        seg_libre c1 conf distance1 in
  let cx, cy, cz = c1 in
  let c1x = vx + cx in 
  let c1y = vy + cy in 
  let c1z = vz + cz in
  let distance1 = distance + 1 in
  seg_libre (c1x,c1y,c1z) conf distance1 ;;

(*Question 23*)

let est_saut (c1:case) (c2:case) (conf:configuration) : bool =
  if associe c2 conf = Libre then
    let list_case, list_color, dim = conf in
    if associe c1 conf = List.hd list_color then
      let vect_unit, distance_c1_c2 = vec_et_dist c1 c2 in
      if distance_c1_c2 mod 2 != 0 then 
        let pivot = calcul_pivot c1 c2 in
        if associe pivot conf != Libre then
          if est_libre_seg c1 pivot conf then
            if est_libre_seg pivot c2 conf then
              true
            else
              false 
          else
            false
        else
          false
      else
        false
    else
      false
  else
    false;;


(*Question 24*)

let est_saut_multiple (list_coup:case list) (conf:configuration) : bool =
  let longueur = List.length list_coup in 
  let rec test list_coup conf i longueur =
    match i with 
    | longueur -> true
    | -1 -> false
    | _ -> let c1 = List.nth list_coup i in 
      let c2 = List.nth list_coup (i+1) in
      if est_saut c1 c2 conf then 
        let i1 = i + 1 in
        test list_coup conf i1 longueur
      else
        let i1 = -1 in
        test list_coup conf i1 longueur in
  test list_coup conf 0 longueur ;;

(*Question 25*)

(*Question 19*)

(* JUSTE *)

type coup = Du of case * case | Sm of case list;;

let est_coup_valide (conf:configuration) (a:coup) : bool =
  let list_case, list_couleur, dim = conf in
  match a with
    | Du(c1,c2) -> if sont_cases_voisines c1 c2 then
        if (associe c1 conf) = (List.hd list_couleur) then
          if associe c2 conf = Libre then
            est_dans_losange c2 dim
          else
            false
        else
          false
      else
        false
    | Sm k -> est_saut_multiple k conf;;

(*Question 20*)

(* JUSTE *)

let appliquer_coup (conf:configuration) (a:coup) : configuration =
  match a with
    | Du(c1,c2) -> let conf1 = supprime_dans_config conf c1 in
      let list_case, list_couleur, dim = conf1 in
      let list_case1 = list_case @ [c2,(List.nth list_couleur 0)] in
      list_case1, list_couleur, dim
    | Sm k -> let c1 = List.hd k in 
      let longueur = List.length k in 
      let c2 = List.nth k (longueur-1) in 
      let conf1 = supprime_dans_config conf c1 in
      let list_case, list_couleur, dim = conf1 in
      let list_case1 = list_case @ [c2,(List.nth list_couleur 0)] in
      list_case1, list_couleur, dim ;;

(*Question 21*)

let mettre_a_jour_configuration (conf:configuration) (cp:coup) : configuration =
  match cp with
  | Du(c1,c2) -> if est_coup_valide conf cp then
      appliquer_coup conf cp
    else
      conf
  | Sm k -> if est_coup_valide conf cp then 
      appliquer_coup conf cp 
    else
      conf ;;



(* Essayer de JIANG Yilun du Question 26*)
(* Il faut utiliser List.forall, List.exists *)
(*let score (conf:configuration) : int =
  let list_case, list_couleur, dim = conf in*)
  
  


  (* pas fini *)


(*Question 26*)

let score (conf:configuration) : int =
  let list_case, liste_joueur, dim = conf in  
  let est_case_joueur (c:case_coloree) : bool = (*cette fonction vérifie si la case coloree entrée est bien de la couleur du joueur actuel*)
    let j1 = List.hd liste_joueur in
    let c1, color = c in
    if color = j1 then 
      true
    else
      false in
  let liste_filtre = List.filter est_case_joueur list_case in
  let i_de_case_coloree (c:case_coloree) = (*cette fonction renvoit la coordonné i d'une case coloree avec case = (i,j,k)*)
    let (i, j, k), color = c in
    i in
  let list_i = List.map i_de_case_coloree liste_filtre in
  let addition (x:int) (y:int) : int =
    x + y in
  let score = List.fold_left addition 0 list_i in 
  score ;;

(*Comme il n'est pas possible qu'un joueur n'est aucun pion sur le plateau la fonction renvoit un score de 0 si un joueur n'a pas de pions mais ce n'est pas gênant*)
assert(score ([(1,2,3),Bleu;(6,8,9),Bleu;(5,-2,-3),Rouge],[Bleu;Rouge],3) = 7);;

let score_gagnant (dim:dimension) : int =
  let triangle_nord = remplir_triangle_bas dim (dim + 1,- dim , - 1) in
  let coord_x_de_i (x:case) : int = (*fait la même chose que la fonction i_de_case_coloree*)
    let i, j, k = x in
    i in
  let list_i = List.map coord_x_de_i triangle_nord in
  let addition (x:int) (y:int) : int =
    x + y in
  let score_gagne = List.fold_left addition 0 list_i in
  score_gagne;;

assert(score_gagnant 3 = 28);; (*renvoit () car le score gagnant pour une dimension 3 est bien de 28*)
assert(score_gagnant 3 = 29);; (*renvoit Assert_failure*)

(*Question 27*)

let gagne (conf:configuration) : bool =
  let score_j = score conf in 
  let list_case, liste_couleur, dim = conf in 
  let score_gagne = score_gagnant dim in 
  score_j = score_gagne;;

assert(gagne ([(28,-28,0),Bleu],[Bleu],3) = true );; (*Renvoit () car le score de bleu est 28*)
assert(gagne ([(29,-28,0),Bleu],[Bleu],3) = true );; (*Renvoit Assert_failure*)

(*Question 28*)

