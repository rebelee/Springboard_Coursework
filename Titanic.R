library(tidyr)
library(dplyr)
library(ggplot2)

# load data into RStudio, clean titanic version from previous project
titanic <- read.csv("titanic_clean.csv")

# replace missing values in survived column with 0
titanic <- titanic %>%
  mutate(survived = replace(survived, is.na(survived), 0L))

# 1 - Check the structure of titanic
# is the data tidy? 
str(titanic)

# 2 - Plot distribution of the sexes within the classes of the ship
ggplot(titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge")

# 3 - Adding facet_grid layer since previous bar plot won't help estimate chance of survival
ggplot(titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Add the final variable Age, and use the position object described above
ggplot(titanic, aes(x = pclass, y = age, col = sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) +
  facet_grid(. ~ survived)