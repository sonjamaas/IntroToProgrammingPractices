##Creating your own functions

addTwoNumbers <- function(x,y){
  z <- x+y
  return(z)
}

addTwoNumbers(5,2)


greet <- function(name){
  paste0("How do you do, ", name, "?")
}

greet("Sonja")


substractTwoNumbers <- function(x,y){
  z <- x-y
  return(z)
}

