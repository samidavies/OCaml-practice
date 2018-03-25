let rec evaporator (content: float) (evapPerDay: float) (threshold: float): int =
    let current = content *.  (1 -. evapPerDay) in 
        if (current < (threshold *. content))
        then 1
        else 1 + (evaporator current evapPerDay threshold)