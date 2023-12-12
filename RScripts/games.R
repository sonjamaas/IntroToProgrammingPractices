install.packages("fun")
library(fun)

if(interactive()){
if (.Platform$OS.type=="windows")x11() else x11 (type="Xlib")
  }
mine_sweeper()

