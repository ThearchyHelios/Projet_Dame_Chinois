type cordonnee = int * int * int;;
let dedans (a:cordonnee):bool = 
  let x, y, z = a in
  if (x + y + z) != 0 then
    false
  else
    if (x <= 3 && x >= -3) && (y <= 3 && y >= -3) then true
    else
      if (y <= 3 && y >= -3) && (z <= 3 && z >= -3) then true
      else
        if (x <= 3 && x >= -3) && (z <= 3 && z >= -3) then true
        else
          false
        ;;
