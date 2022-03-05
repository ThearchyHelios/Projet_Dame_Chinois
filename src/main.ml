type cordonnee = int * int * int;;
type dimension = int;;
let dimension = 3;;

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


(* type couleur = Vert | Rouge | Bleu | Jaune | Marron | Noir;; *)

let check_direction cordonnee =
  let dim = dimension in
  match cordonnee with
  | (x, y, z) when x > dim && y < 0 && z < 0 -> "Nord"
  | (x, y, z) when x > 0 && y > 0 && z < -dim -> "Nord_Ouest"
  | (x, y, z) when x < 0 && y > dim && z < 0 -> "Sud_Ouest"
  | (x, y, z) when x < -dim && y > 0 && z > 0 -> "Sud"
  | (x, y, z) when x < 0 && y < 0 && z > dim -> "Sud_Est"
  | (x, y, z) when x > 0 && y < -dim && z > 0 -> "Nord_Est"
  | (x, y, z) when x = 0 && y = 0 && z = 0 -> "Point Centre"
  | _ -> "Zone Centrale";;

check_direction (0, -3, 3);;

(* Question 2 *)
let a:cordonnee = (0, 0, 0) in
if check_direction a = "Point Centre" || check_direction a = "Zone Centrale" || check_direction a = "Nord" || check_direction a = "Sud" then
  "Question 2 Verifiee"
else
  "Question 2 Failed";;


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

tourner_case (-3, 2, 1) 3;;
