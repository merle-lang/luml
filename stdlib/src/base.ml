module Base

[%lua

local _toString
_toString = function(value)
  local t = type(value)
  if t == 'function' then
    return '<function>'
  elseif t == 'nil' then
    return 'unit'
  elseif t == 'table' then
    if value._type == 'tuple' then
      local acc = ''
      for key, v in ipairs(value) do
        if acc ~= '' then
          acc = acc .. ', '
        end
        acc = acc .. _toString(v)
      end
      return '(' .. acc .. ')'
    elseif value._type == 'adt' then
      local acc = ''
      for key, v in ipairs(value) do
        if acc ~= '' then
          acc = acc .. ' '
        end
        local sub = _toString(v)
        if string.find(sub, " ") ~= nil then
          sub = '(' .. sub .. ')'
        end
        acc = acc .. sub
      end
      return acc
    elseif value._type == 'record' then
      local acc = '{'
      for key, v in pairs(value) do
        if key ~= '_type' then
          if acc ~= '{' then
            acc = acc .. ','
          end
          acc = acc .. key .. '=' .. _toString(v)
        end
      end
      return acc .. '}'
    elseif value.value ~= nil then
      local acc = ''
      local el = value
      while el.value ~= nil do
        if acc ~= '' then
          acc = acc .. ', '
        end

        acc = acc .. _toString(el.value)
        el = el.next
      end

      return '[' .. acc .. ']'
    elseif t == 'string' then
      return '"' .. value .. '"'
    end

    return "[]"
  else
    return tostring(value)
  end
end

_catString = function(a, b)
  return a .. b
end

%]

(+) : Int -> Int -> Int
(+) a b = @lua.+ a b

(-) : Int -> Int -> Int
(-) a b = @lua.- a b

(*) : Int -> Int -> Int
(*) a b = @lua.* a b

-- Integer division. In Lua 5.3 we get //,
-- but for backwards-compatability we emulate it
(/) : Int -> Int -> Int
(/) a b = @math.floor (@lua./ a b)

(+.) : Float -> Float -> Float
(+.) a b = @lua.+ a b

(*.) : Float -> Float -> Float
(*.) a b = @lua.* a b

(|>) : a -> (a -> b) -> b
(|>) x f =
  f x

(<|) : (a -> b) -> a -> b
(<|) f x =
  f x

(::) a b = a :: b

(==) : a -> a -> Bool
(==) a b = @lua.== a b

(/=) : a -> a -> Bool
(/=) a b = @lua.~= a b

(&&) : Bool -> Bool -> Bool
(&&) a b = @lua.and a b

(||) : Bool -> Bool -> Bool
(||) a b = @lua.or a b

(++) : String -> String -> String
(++) a b = @lua._catString a b

(>) : a -> a -> Bool
(>) a b =
  @lua.> a b

(<) : a -> a -> Bool
(<) a b =
  @lua.< a b

mod : Int -> Int -> Int
mod a b = @lua.% a b

range : Int -> Int -> List Int
range a z = 
  match a == z with 
    true -> [] 
  | false -> a :: (range (a+1) z) 
  end

toString : a -> String
toString x = @lua._toString x

always : a -> b -> a
always value x = value

log : String -> a -> a
log tag value = @lua.print (tag ++ ": " ++ (toString value))

flip : (a -> b -> c) -> b -> a -> c
flip f a b =
  f b a

(>>) f g x =
  g (f x)

fst : (a, b) -> a
fst x = 
  let (first, _) = x in first

snd : (a, b) -> b
snd x =
  let (_, second) = x in second
