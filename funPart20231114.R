install.packages("devtools")
if(!require(devtools)) {install.packages("devtools")}
devtools::install_github("brooke-watson/BRRR")
library(BRRR)

skrrrahh(8)

f <- function(sound, sleep=0.75){
  Sys.sleep(sleep)
  BRRR::skrrrahh(sound)
}

for(i in 1:5){
  f(i)
}

purrr::walk(30:35,f)

devtools::install_github("coolbutuseless/devout")
devtools::install_github("coolbutuseless/devoutaudio")
