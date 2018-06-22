module List

import Base ((::), (|>), (+), (==), (&&), always)
import Option (Option)

foldl : (a -> b -> b) -> b -> List a -> b
foldl f init items = 
  match items with
    [] -> init 
    | h :: t -> 
      let nextAcc = (f h init) in
      foldl f nextAcc t
  end

reverse : List a -> List a
reverse = foldl (::) []

map : (a -> b) -> List a -> List b
map f items = 
  foldl (\i acc -> (f i) :: acc) [] items 
  |> reverse

mapi : (Int -> a -> b) -> List a -> List b
mapi f items =
    let (_, result) = foldl (\item acc -> 
      let (i, accItems) = acc in
        (i + 1, ((f i item) :: accItems)))
    (0, []) items
    in reverse result

get_ i items counter =
  match items with
     [] -> Option.None
  | item :: rest ->
      match i == counter with
        true -> Option.Some item
      | false -> get_ i rest (counter + 1)
      end
  end

get : Int -> List a -> Option a
get i items =
  get_ i items 0

hd : List a -> Option a
hd = get 0

filter : (a -> Bool) -> List a -> List a
filter pred = 
  foldl (\item acc -> 
    match pred item with 
      true -> item :: acc
    | false -> acc 
    end)
  []

length : List a -> Int
length =
  foldl (always ((+) 1)) 0

concat : List a -> List a -> List a
concat l1 l2 =
  foldl (::) (reverse l1) l2
  |> reverse

all : (a -> Bool) -> List a -> Bool
all pred items =
  foldl (\item acc -> (pred item) && acc) true items

