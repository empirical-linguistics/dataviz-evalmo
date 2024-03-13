# install.packages("tidyverse")
# install.packages("ggbeeswarm")
library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(patchwork)

# read data
d <- read_xlsx("../data/fake_RT_data.xlsx")

# read another file
d1 <- read_csv("../data/dtababy_with_author_data.csv")


# piping
(2+2) %>% sqrt()
sqrt(4)
gsub(pattern = "bla", replacement = "blubb", x = "bla bla bla")
d1$Year <- gsub(".*_", "", d1$ID)
d1 <- d1 %>% mutate(Year2 = gsub(".*_", "", ID))

# pivoting
d <- d %>% pivot_longer(cols = starts_with("ReactionTime"))
d <- setNames(d, c("Participant", "Condition", "RT"))
d$Condition <- gsub(".*_Condition", "", d$Condition)

# read data
d <- read_xlsx("../data/fake_RT_data.xlsx")
d <- d %>% pivot_longer(cols = starts_with("ReactionTime"),
                   names_prefix = "ReactionTime_Condition",
                   names_to = "Condition",
                   values_to = "RT")

# plot

p1 <- ggplot(d, aes(x = Condition, y = RT)) +
  geom_beeswarm() + geom_boxplot(alpha = .7) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 18, family = "Times"),
        axis.text = element_text(size = 18, family = "Times")) 


(p <- ggplot(d, aes(x = Condition, y = RT)))
(p <- p + geom_beeswarm())

p /
  p1
ggsave("example.png", width = 9, height = 13)
ggsave("example.svg")



# more fake data ----------------------------------------------------------

set.seed(42)
df <- tibble(x = 1:100,
       y = rnorm(100))


ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth()





