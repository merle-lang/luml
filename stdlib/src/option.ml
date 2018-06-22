module Option

type Option a = Some a | None

default d o = 
    match o with 
        Some x -> x 
      | None -> d 
    end