library(tidyverse)
library(beeswarm)
library(ggbeeswarm)
library(scales)
library(patchwork)

###################################
# Data visuazlization with base R #
###################################

# simple scatterplot
data("cars")

plot(x= cars$speed, y = cars$dist)

plot(x = cars$speed,    # x axis 
     y = cars$dist,     # y axis
     pch = 20,          # optional: change point type
     col = "blue",      # optional: change color
     xlab = "speed",    # x axis label
     ylab = "distance") # y axis label
abline(lm(cars$dist~cars$speed), # add regression line
    col = "grey",                # color
    lty = 2)                     # line type (2 = dashed) 


d <- data.frame(x = 1:10,
                y = sample(1:100, 10))

plot(d$x, d$y, ylim = c(0, max(d$y)))
title("Some plot")


# alternative: lowess curve
# lines(lowess(cars$speed, cars$dist)) # add lowess curve

title("Cars")           # add title


# simple lineplot ---------------------------------------------------------

data("WWWusage")
plot(1:100, WWWusage, type = "l", xlab="Time",
     main = "WWW usage per minute")

# we can also combine dotplot and lineplot:
plot(1:100, # x axis
     WWWusage, # y axis
     type = "p", # p = points
     xlab="Time", # x axis label
     pch = 20, # change point type
     col = rgb(0,0,1,.25), # RGB: red, green, blue + alpha (= transparency)
     ylim = c(0,250),  # y axis limits
     main = "WWW usage per minute", cex=2, cex.axis=3, cex.main=3, cex.lab=3)
points(x=20,y=150, col="red")
text(x=80,y=200, "hi", cex=3)
lines(1:100, WWWusage, 
      type = "l", # l = line
      col = rgb(.256,.256,.256,.5), # RGB code for grey (+ alpha)
      lwd = 4)

# we can also add a grid:
grid(nx = 0, # no grid lines on x axis
     ny = NULL, # NULL here means that grid aligns with tick marks
                # (see vignette: ?grid)
     lty = 1, col = "grey90")

# simple barplot ----------------------------------------------------------

# set a seed for replicability
set.seed(utf8ToInt("lotwinterschool"))

# get a sample
spl <- LETTERS[round(rnorm(200, mean = 5, sd = 2))]


# barplot
table(spl) %>%  # tabulate
  barplot()

# we can also sort the table by frequency
# before plotting:
table(spl) %>% sort(decreasing = T) %>% barplot()

# we can also add absolute numbers:
tbl <- table(spl) %>% sort(decreasing = T) # save table as object
(bp <- barplot(tbl))
text(bp,    # x axis values taken from bp object (i.e. the barplot itself)
     tbl/2, # y axis value taken from tbl, divided by two
            # to be in the middle of the bars
     tbl,   # labels taken from the table 
     cex = .8) # decrease font size a bit
title("My beautiful barplot") # add title

# x and y labels can also be added using the title() command:
title(xlab = "Letter", ylab = "Frequency")


# stacked barplot
# possible in base R but MUCH (!) easier in 
# ggplot2, hence we will deal with this later.



# boxplot -----------------------------------------------------------------

# sample data
set.seed(42)
d <- tibble(x = rnorm(100, mean = 5, sd = 1),
            y = rnorm(100, mean = 7, sd = 3))


boxplot(d)

# Interpretation: the box shows the interquartile
# range, i.e. the area in which the middle 50% of
# the data fall. The whiskers show 1.5 times the
# interquartile range; data that fall out of this
# range are presented as outliers (individual dots).
quantile(d$y)
IQR(d$y)

# beeswarm plot -----------------------------------------------------------

beeswarm(d,                # x axis
         pch = 20,         # y axis
         col = "darkblue", # color of the points
         method = "hex")   # methods for distributing the dots
                           # (chosen here to avoid overlaps
                           # that we see with the default method)
boxplot(d, add = T, col = rgb(.256, .256, .256, .2))




# exporting base R plots --------------------------------------------------

# base R plots can be exported using the png function:
# wrap the plot in
# png(...)
# PLOT COMMANDS
# dev.off()
png("exampleplot.png",
    width = 6.5, height = 5, 
    un = "in", 
    res = 300)
beeswarm(d,                
         pch = 20,         
         col = "darkblue", 
         method = "hex")
boxplot(d, add = T, col = rgb(.256, .256, .256, .2))
dev.off()



# plot grids --------------------------------------------------------------

# Multiple plots can be exported by setting up a 
# grid of n rows x n columns using par(mfrow = x,y)
png("exampleplot02.png", width = 11.5, height = 5, un = "in", res = 300)
par(mfrow = c(1,2)) # 1 row, two columns
beeswarm(d,                
         pch = 20,         
         col = "darkblue", 
         method = "hex")
boxplot(d, col = rgb(.256, .256, .256, .2))
dev.off()
par(mfrow = c(1,1)) # restore default



####################################
# Data visuazlization with ggplot2 #
####################################


# simple scatterplot ------------------------------------------------------

ggplot(cars,          # the dataset
      aes(           # IMPORTANT: the aesthetic mappings, i.e. the
          x = speed, # mappings between data and visual properties,
          y = dist         # have to be wrapped in aes()!
      )) +
  geom_point()     # a geom object that describes
                   # how to render the observation.


# The plot can be saved as an object and then be
# customized further by adding more geoms as well as
# so-called SCALES, which can be used to modify the
# appearance of each geom layer.

p1 <- ggplot(cars,          
       aes(           
         x = speed, 
         y = dist   
       )) +
  geom_point() 

# add a regression line:
(p1 <- p1 + 
  geom_smooth(method = "lm") )

# change appearance of x axis:
# (just for illustrative purposes, doesn't make sense here)
p1 + scale_x_continuous(breaks = seq(0,25,7))

# log-transform y axis (also just for demonstraion):
p1 + scale_y_continuous(trans = "log2")

# with the scale_ arguments, we can change the
# aesthetic mappings specified via aes(). Why does this
# not work?
p1 + scale_color_manual(values = "red")

# Because we haven't specified color in the aesthetic
# mappings. We could do so:
(p1 <- p1 + geom_point(aes(color = "bla")))
(p1 <- p1 + scale_color_manual(values = "red"))

# remove legend:
(p1 <- p1 + guides(col = "none"))

# we can also customize x and y labels by adding
# the respective layers
(p1 <- p1 + xlab("Speed") + ylab("Dist"))

# ... and add a title:
(p1 <- (p1 + ggtitle("Cars")))

# ... and customize the title, e.g.
# to make it boldface and center it:
(p1 <- p1 + theme(plot.title = element_text(face = "bold", 
                                     hjust = 0.5)))


# we can also change the theme:
p1 + theme_bw()
p1 + theme_minimal()

# or remove or cutomize the gridlines:
p1 + theme(panel.grid.minor = element_blank())
p1 + theme(panel.grid.major = element_blank())
p1 + theme(panel.grid = element_blank())
p1 + theme(panel.grid.major.x =  element_blank(),
           panel.grid.minor.x = element_blank())

# simple lineplot ---------------------------------------------------------

# ggplot only accepts dataframes as input,
# so we first have to build a dataframe:
wwwusage <- tibble(time = 1:100,
       WWWusage = WWWusage)

# now try building the plot yourself!
# Hint: the relevant geom is geom_line().

ggplot(wwwusage, aes(x = time, y = WWWusage)) +
  geom_point() +
  geom_line(lwd=1.5)




# barplot -----------------------------------------------------------------

# again, remember that ggplot usually only accepts
# dataframes as input. One thing we can do is just
# creating a one-column dataframe with our spl data
# from above:
tibble(x = spl) %>% 
  ggplot(aes(x = x)) + 
  geom_bar() # barplot

# alternatively, we can use the already tabulated data
# with geom_col():
tbl <- as_tibble(tbl)

# now we can use this as input for geom_col:
colnames(tbl)
ggplot(tbl, aes(x = spl,  # x axis
                y = n)) + # y axis
geom_col() # same result as before.


# stacked barplot

# add a group variable to spl: letters from A-E vs. letters
# from F-K
spl01 <- tibble(letters = spl)
spl01$group <- ifelse(spl01$letters %in% LETTERS[1:5], "A-E", "F-K")

# plot:
spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar() +
  scale_fill_viridis_d() # colorblind-friendly palette

# we can also add labels:
spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar() +
  scale_fill_viridis_d() +
  geom_text(stat="count", # necessary to compute y axis position
                          # (horribly counterintuitive, I know!!)
            aes(label = letters), 
            position = position_stack(vjust = .5),
            col = c(rep("white", 5), rep("black", 6))) + # specify color of text
  guides(fill = "none") # remove legend
  

# using proportions:
(p <-spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) ) # requires package "scales"!

# add labels:
p + geom_text(stat="count", 
              aes(label = after_stat(count)), 
              position = position_fill(vjust = .5))



# beeswarm and boxplots ---------------------------------------------------

# we use the d dataframe again that
# we created above.
head(d)

# first try:
ggplot(d, aes(x = x,
              y = y)) + geom_beeswarm()

# Doesn't look quite right. Why not?
# Because x and y are different groups!
# We have to transform the data first:
# One column for the values, one for the groups.
# This can be done using pivot_longer:

d <- pivot_longer(d, cols = c("x", "y"))
d

# replace the names:
colnames(d) <- c("group", "value")


# now let's try again:
ggplot(d, aes(x = group, y = value)) +
  geom_beeswarm()

# looks much better!
# again, we can overlay other plots:

(p <- ggplot(d, aes(x = group, y = value)) +
    geom_beeswarm(col = "blue"))

# add boxplot:
p + geom_boxplot(fill = "grey", alpha = .3)

# or a violin plot:
p + geom_violin(alpha = .3)



# exporting ggplots -------------------------------------------------------

# We can use ggsave to save ggplots:
p + geom_violin(alpha = .3)
ggsave("exampleggplot.png")



# multiple ggplots --------------------------------------------------------

# We can use the excellent patchwork package
# to group ggplots:

p | p1
ggsave("exampleggplot2.png", width = 12, height = 5.5)


# It often helps to use bigger axis labels
# because the default ones tend to be hard to read
# in presentations or publications.
# In fact, I have saved a code snippet in RStudio
# (see Tools > Global Options > Code > Code Snippets)
# to have it always available:

p + theme(text = element_text(size = 18),
          title = element_text(size = 18))

# Note that you have to use it *after* changing
# the global theme. If you switch to e.g. theme_bw or
# theme_minimal afterwards, your changes to the theme
# will be overridden:

p + theme(text = element_text(size = 18),
          title = element_text(size = 18)) +
  theme_bw() # font size change is lost

# So the order in which you use the commands matters!



