install.packages("devtools")
if(!require(devtools)) {install.packages("devtools")}
devtools::install_github("brooke-watson/BRRR")

library(BRRR)

skrrrahh("schoolboy")

f <- function(sound, sleep=0.75){
  Sys.sleep(sleep)
  BRRR::skrrrahh(sound)
}

for(i in 1:10){
  f(i)
}

purrr::walk(30:35,f)

skrrrahh_list()
