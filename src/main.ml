type cordonnee = int * int * int;;
type dimension = int;;
let dimension = 3;;

let check_dimension dimension = match dimension with
| _ when dimension < 0 -> false
| _ when dimension > 0 -> true
| _ -> false;;

(* check_dimension (-4);; *)
let est_dans_etoile (a:cordonnee) dimension :bool =
  let x, y, z = a in
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


if check_dimension dimension then
  let a = (0, 0, 0) in
  let b = dimension in
  est_dans_etoile a b
else
  false;;

est_dans_etoile (0, 0, 0) dimension;;


(* type couleur = Vert | Rouge | Bleu | Jaune | Marron | Noir;; *)

