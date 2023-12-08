##Creating your own functions

myfunction <- function(x,y){
  z <- x+y
  return(z)
}

myfunction(5,2)


greet <- function(name){
  paste0("How do you do, ", name, "?")
}

greet("Sonja")
