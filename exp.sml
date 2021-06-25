
fun exp (0:int):int = 1
  | exp n = 2 * exp(n-1)

val 1 = exp 0
val 8 = exp 3
val 131072 = exp 17
