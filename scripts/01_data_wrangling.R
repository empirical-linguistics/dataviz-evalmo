# install.packages("tidyverse")
library(tidyverse)
library(readxl)

# Vectors
my_vector <- c(1,2,3)
my_character_vector <- c("a", "b", "c")
my_other_character_vector <- c(1, "two", 3)

# Data frames
my_data_frame <- data.frame(names = c("Jack", "Jill", "Hensel", "Gretel"),
           gingerbread = c(5, 6, 2, 10) )

my_data_frame[2,2]
my_data_frame[3,1]
rownames(my_data_frame)

my_second_data_frame <- data.frame(A = my_vector, B = my_character_vector, 
           C = my_other_character_vector)


# Matrices
my_matrix <- matrix(c(1,2,3,4,5,6), ncol = 2)


# Lists
my_list <- list(my_vector, my_data_frame, my_matrix)

my_vector[2]
my_list[[2]][3,2]


# logical
my_logical_object <- my_list[[2]][3,2] == 2
test <- 2
test2 = 2
test == test2

# character
my_character_object <- "hi"

# numeric
my_numeric_object <- sqrt(5)

# integer
my_integer_object <- 5
my_integer_object <- as.integer(my_integer_object)
is.integer(my_integer_object)

# factors
my_data_frame2 <- my_data_frame
my_data_frame$names <- factor(my_data_frame$names)
str(my_data_frame)
my_data_frame2[5,] <- c("James", 4)
my_data_frame2

my_data_frame[5,] <- c("James", 4)
my_data_frame <- my_data_frame[-5,]
my_data_frame$names
my_data_frame$names <- factor(my_data_frame$names, levels = c("Hensel", "Gretel", "Jack", "Jill"))
my_data_frame$names


# Loops
squareroots <- NA

for(i in 1:100) {
  squareroots[i] <- sqrt(i)
}


# Functions
get_squareroot <- function(range) {
  
  squareroots <- NA
  
  for(i in range) {
    squareroots[i] <- sqrt(i)
  }
  
  return(squareroots)
  
}


# apply
sapply(1:100, function(i) sqrt(i))



######################################
## concrete application:  chen/lein ##
######################################

# read data
d <- read_csv("../data/chen_lein.csv.zip", quote = "\"")

# structure of the data frame
str(d)
colnames(d) # tag0002 is the lemma column

# rename the column (using dplyr's rename_with)
d <- rename_with(d, ~"Lemma", "tag0002")

# add a column indicating whether the lemma
# ends with chen or lein
d$Variant <- ifelse(grepl(".*lein$", d$Lemma), "lein", "chen")

# plot relative frequency of variant over time
d %>% group_by(file_decade, Variant) %>% summarise(
  n = n()
) %>% ggplot(aes(x = factor(file_decade), y = n, fill = Variant)) +
  geom_col(position = position_dodge())

# or stacked barplot with bars summing up to 100%
d %>% group_by(file_decade, Variant) %>% summarise(
  n = n()
) %>% ggplot(aes(x = factor(file_decade), y = n, fill = Variant)) +
  geom_col(position = "fill")

# we can also group by text type
d %>% group_by(file_decade, file_genre, Variant) %>% summarise(
  n = n()
) %>% na.omit %>% 
  ggplot(aes(x = factor(file_decade), y = n, fill = Variant)) +
  geom_col(position = "dodge") + facet_wrap(~file_genre)




