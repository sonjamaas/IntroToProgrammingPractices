install.packages("ggplot2")
library(ggplot2)
x11()
x <- data.frame(x=1,y=1,label="ggplot2 introduction \n@ EAGLE")

ggplot(data=x, aes(x=x,y=y))+geom_text(aes(label=label, colour="pink"),size=15)


#creating data for excercise

x1 <- rnorm(1000,0,1)
x2 <- rnorm(1000,5,10)
x3 <- rep(c('blue','red','pink','green','yellow','purple'),200)[1:1000]
x4 <- rnorm(1000,0,1)

df <- data.frame(a=x1,b=x2,c=x3,d=x4)
df
library(ggplot2)
ggplot(df,aes(a,b,colour=c))+
  geom_point(alpha=.5)+labs(title="First plot", x="x axis \n and a new line")


ggplot(df, aes(a))+ 
  geom_histogram(color="white")

ggplot(df, aes(a))+ 
  geom_density()


#combining plots
ggplot(df)+  
  geom_histogram (aes(a,after_stat(density)), fill="blue", colour="darkgrey")+ 
  geom_density(aes(a,after_stat(density)),colour="yellow")+
  geom_rug(aes(a))

ggplot(df,aes(c,color=c))+
  geom_point(stat="count",size=4)
ggplot(df)+
  geom_bar(aes(c))+
  coord_flip()
ggplot (df,aes(d,a))+
  geom_boxplot()
ggplot(df,aes(d,a))+
  geom_boxplot()+
  geom_jitter(alpha=.5,width=.3,color="hotpink")


#3d plot
# library
install.packages("rgl")
library(rgl)

# This is to output a rgl plot in a rmarkdown document.
# setupKnitr()

# Data: the iris data is provided by R
#data <- iris

# Add a new column with color
#mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
#data$color <- mycolors[ as.numeric(data$Species) ]


# Plot
plot3d( 
  x=df$`a`, y=df$`b`, z=df$`d`, 
  col = df$c, 
  type = 's', 
  radius = 1,
  xlab="a", ylab="b", zlab="d")

# To display in an R Markdown document:
# rglwidget()

# To save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                        file = "HtmlWidget/3dscatter.html",
                        libdir = "libs",
                        selfcontained = FALSE
)

#donut plot
doughnut <-
  function (x, labels = names(x), edges = 200, outer.radius = 0.8,
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL,
            angle = 45, col = NULL, border = FALSE, lty = NULL,
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        palette()
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p),
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i],
              angle = angle[i], border = border[i],
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
             ...)
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i],
              angle = angle[i], border = border[i],
              col = "white", lty = lty[i])
    }
    
    title(main = main, ...)
    invisible(NULL)
  }


# Let's use the function, it works like PiePlot !
# inner.radius controls the width of the ring!
doughnut( df$a, inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), rgb(0.2,0.9,0.4,0.4) , rgb(0.0,0.9,0.8,0.4)) )

