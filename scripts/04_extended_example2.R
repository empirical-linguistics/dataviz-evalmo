library(tidyverse)

# read data ---------------------------------------------------------------

d <- read_csv("../data/bnc_ish.csv.gz")



# find hapaxes ------------------------------------------------------------

hapaxes <- table(d$lemma) %>% as.data.frame() %>% filter(Freq == 1) %>%
  select(Var1) %>% unlist() %>% as.character()


# get ttr and pp ----------------------------------------------------------

d_tbl <- d %>% filter(text_publication_date != "unknown") %>%
  group_by(text_publication_date) %>%
  summarise(
    types = length(unique(lemma)),
    tokens = n(),
    ttr = types / tokens,
    hapax_count = length(which(lemma %in% hapaxes)),
    pp = hapax_count / tokens
  )



