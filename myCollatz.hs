collatz :: Int -> Int
collatz 1 = 1
collatz a =
  if odd a
    then 1 + collatz (3 * a + 1)
    else 1 + collatz (div a 2)