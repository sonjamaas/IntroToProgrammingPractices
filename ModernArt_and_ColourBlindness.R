devtools::install_github("gsimchoni/kandinsky")
library(kandinsky)

kandinsky(Indometh)
data()


library(ggplot2)

e=1e-3
s=1e4
t=pi/15*cumsum(seq(e,-e,length.out=s))^4
x=ggplot()+
  geom_spoke(aes(x=cumsum(cos(t)),
                 y=cumsum(sin(t)),
                 angle=t,color=t,
                 radius=1:s%%300),
             alpha=0.5)+
  scale_color_distiller(palette=15,guide="none")+
  coord_fixed()+
  theme_void()

devtools::install_github("wilkelab/cowplot")
install.packages("colorspace",repos="http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")
library(colorblindr)
cvd_grid(x)
