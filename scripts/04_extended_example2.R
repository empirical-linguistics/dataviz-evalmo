library(tidyverse)
library(vroom)
library(viridis)

# read data ---------------------------------------------------------------

d <- read_csv("../data/bnc_ish.csv.gz")
# vroom("../data/bnc_ish.csv.gz")


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

ggplot(d_tbl, aes(x = text_publication_date, y = pp, 
                  label = paste0("total # of tokens: \n", tokens))) +
  geom_col(aes(fill = text_publication_date)) +
  geom_text(vjust = 1.5, col = "white") +
  scale_fill_viridis_d("magma", end = .8) +
  guides(fill = guide_legend(title = "Time slice")) +
  xlab("Publication date") + ylab("Potential productivity") +
  ggtitle("Productivity of -ish") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("prod_ish.png", dpi = 1200)  





