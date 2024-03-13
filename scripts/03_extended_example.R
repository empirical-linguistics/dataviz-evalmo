library(tidyverse)
library(shiny)


# read data
d1 <- read_delim("../data/ungbaby.csv", 
                 delim = "\t", 
                 quote = "") # concordance
d2 <- read_csv("../data/dtababy_with_author_data.csv") # metadata


# combine datasets --------------------------------------------------------

# inspect datasets
colnames(d1) 
colnames(d2)

# The column "text_name" in d1 contains the 
# DTA IDs, but with "text_name " in each row!
# So we have to remove this using the replacement
# (substitute) command gsub():

d1 <- mutate(d1, ID = gsub("text_name ", "", d1$Text_Name))

# now we can easily join the datasets:
d <- left_join(d1,d2)

# check that the number of rows is identical:
nrow(d) == nrow(d1) # of course you can also do this
                    # by just looking at the environment
                    # in the upper-right corner of RStudio!

# inspect new dataset
colnames(d)


# calculate morphological productivity ------------------------------------

# add decade column
d$Decade <- gsub(".$", "5", d$Year)

# check
unique(d$Decade) %>% sort # looks good

# get hapax legomena
hapaxes <- as.data.frame(table(d$Lemma)) %>% 
  filter(Freq == 1) %>% select(Var1)

# note that it is still a df!
str(hapaxes)

# we want a vector:
hapaxes <- as.vector(hapaxes[['Var1']])

# or:
#hapaxes <- as_tibble(table(d$Lemma), .name_repair = "unique") %>%
 # setNames(c("Lemma", "Freq")) %>% select(Lemma) %>% as.vector()




# summarise
d_tbl <- d %>% group_by(Decade, Text_Type) %>% summarise(
  number_of_types  = length(unique(Lemma)),
  number_of_tokens = n(),
  number_of_hapaxes = length(which(Lemma %in% hapaxes)),
  ttr = number_of_types / number_of_tokens,
  pp = number_of_hapaxes / number_of_tokens
)

# as above, remove the "text_texttype " from
# the Text_Type column:
d_tbl$Text_Type <- gsub("text_texttype ", "", d_tbl$Text_Type)

# visualize productivity development

ggplot(d_tbl, aes(x = Decade, y = pp, 
                  col = Text_Type, 
                  group = Text_Type)) +
  geom_point() +
  geom_line()
  
# or:

ggplot(d_tbl, aes(x = Decade, y = pp, 
                  group = 1, 
                  col = Text_Type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Text_Type) +
  guides(col = "none") +
  theme(axis.text.x = element_text(angle=45, hjust=.9, size=12)) +
  ylab("Potential Productivity")
  
