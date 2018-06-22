module String

import Base ((++))

-- Join parts of strings together
join : String -> List String -> String
join delim parts =
  match parts with 
      [] -> "" 
    | [hd] -> hd 
    | hd :: tail -> hd ++ delim ++ (join delim tail) 
  end