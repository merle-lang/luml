module File

import Base (toString)
import Result (Result)

[%lua

local __read_file = function(filename)
  local file = io.open(filename, "rb")
  if not file then return {false, "File does not exist"} end
  local content = file:read "*a"
  file:close()
  return {true, content}
end

%]

read : String -> Result String String
read filename =
  match (@lua.__read_file filename) with
    (true, data) -> Result.OK data
  | (false, error) -> Result.Error error
  | other -> Result.Error (toString other)
  end
