install.packages("babynames")
library(babynames)

#variables
temp1 <- filter(babynames,sex=="M",name=="Taylor")
temp2 <- select(temp1,n)
temp3 <- sum(temp2)

#nested
sum(select(filter(babynames,sex=="M",name=="Taylor"),n))

#pipes
babynames %>% 
  filter(sex=="F",name=="Sonja") %>%
  select(n) %>%
  sum()


