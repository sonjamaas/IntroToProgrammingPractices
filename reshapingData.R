##Excersise on reshaping data

# create the data
fielddata_wide <- read.table(header=TRUE, text='
                             plot_id name Cover LAI DBH
                             1 Sophie 79 2.3 1.7
                             2 Achmed 63 0.6 1.1
                             3 Achmed 95 3.1 1.8
                             4 Sophie 11 3.4 1.9
                             ')

# look at the data
fielddata_wide

# make sure the plot id is a factor
fielddata_wide$plot_id <- factor(fielddata_wide$plot_id)

## reshape data with melt

# activate the needed package
library(reshape2)

# change the format to a long version
# plot_id and names should be preserved
melt(fielddata_wide, id.vars=c("plot_id","name"))

# check what happens if you exclude "name"
melt(fielddata_wide, id.vars=c("plot_id"))

# same as before but more details and save to new object
fielddata_long <- melt(fielddata_wide,
                       #ID variables - all the variables to keep but not split apart on
                       id.vars = c("plot_id","name"),
                       #the source columns
                       measure.vars = c("Cover","LAI","DBH"),
                       #name of the destination column that will identify the original column that the measurement came from
                       variable.name="method",
                       value.name="measurement"
                       )

#look at the data
fielddata_long

##assume you created field data differently and reshape with dcast()
fielddata_long2 <- read.table(header=TRUE, text='
plot_id name sample measurement
                              1 Ahmed training 7.9
                              1 Ahmed valid1 12.3
                              1 Sophie valid2 10.7
                              2 Sophie training 6.3
                              2 Sophie valid1 10.6
                              2 Sophie valid2 11.1
                              3 Sophie training 9.5
                              3 Sophie valid1 13.1
                              3 Sophie valid2 13.8
                              4 Ahmed training 11.5
                              4 Ahmed valid1 13.4
                              4 Ahmed valid2 12.9
                              ')

# make sure plot_id column is a factor
fielddata_long2$plot_id <- factor(fielddata_long2$plot_id)

#need it more condensed: sample type in columns
fielddata_wide2 <- dcast(fielddata_long2, plot_id+name~sample, value.var="measurement")
fielddata_wide2
