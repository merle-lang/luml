module Result

-- Result type

type Result error value = OK value | Error error

map f result =
    match result with
      OK something -> OK (f something)
    | Error _ -> result
    end
