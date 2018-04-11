# load packages
library(tidyr)
library(dplyr)
library(readr)

# load data into RStudio
anime <- read.csv("anime.csv")
rating <- read.csv("rating.csv")

# 1. CLEAN UP ANY COLUMN NAMES
colnames(anime)
colnames(rating)
# fix rating in anime to show it's the mean of ratings instead
names(anime)[names(anime) == 'rating'] <- 'avg_rating'
head(anime, 10)

# 2a. CHECK FOR MISSING VALUES
# first in the anime data
colSums(is.na(anime))
# can see there are 230 NA values in the avg_rating column
# replace NA values from avg_rating with -1 to show no value instead
anime <- anime %>%
  mutate(avg_rating = replace(avg_rating, is.na(avg_rating), -1))
anime %>% count(avg_rating)
# double check it worked
colSums(is.na(anime))

# do the same for ratings data
colSums(is.na(rating))
# don't need to go further since there are no NA values in this data set

# check if there are blank values
colSums(anime == '')
# can see there are 62 blank in genre, 25 blank in type
# it seems that if there is no type it meant it wasn't released yet (based on the website)
# so remove the rows where there is no type since it won't change the result
anime <- subset(anime, type!='')
colSums(anime == '')
# now what do the missing genres mean?

# check blank values for rating as well
colSums(rating == '')
# there are none so we can move on

# 2b. DO VALUES MAKE SENSE?
# take a look at the data's structure
str(anime)
# looks like something weird is going on with the episodes type, should be int
# this looks like it's because there are values called "Unknown" which need to be replaced
anime %>%
  group_by(episodes) %>%
  summarise(eps = n(), anime = n_distinct(anime_id)) %>%
  filter(episodes == "Unknown")

# change episodes from factor to int, UNKNOWN automatically replaced by NA
anime$episodes <- as.numeric(levels(anime$episodes))[anime$episodes]
str(anime$episodes)
colSums(is.na(anime))

# take a look at the mean, median, and mode of episodes
mean(anime$episodes, na.rm = TRUE) # 12.38
median(anime$episodes, na.rm = TRUE) # 2

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(anime$episodes) # 1

anime %>%
  group_by(episodes) %>%
  filter(is.na(episodes)) %>%
  View()

# Mode is 1, mean is 12.38 and median is 2. Which one do you pick?
# first, take a look at anime types and if we can replace NA values based on these 
# (e.g. NA replaced with 1 if the type is a movie)
# does mean/median/mode change based on these?

# mean: movie 1.10, music 1.13, ONA 6.88, OVA 2.42, Special 2.56, TV 35.92
sapply(split(anime$episodes, anime$type), mean, na.rm = TRUE)
# median: movie 1, music 1, ONA 2, OVA 2, special 1, TV 24
sapply(split(anime$episodes, anime$type), median, na.rm = TRUE)
# mode: movie 1, music 1, ONA 1, OVA 1, special 1, TV 12
sapply(split(anime$episodes, anime$type), Mode)

# all quite different in terms of mean, so take a look at each type separately to decide 
# whether to replace with mean or median

# mean and median are about the same value so replace NA values with one if type is movie
anime <- anime %>%
  mutate(episodes = ifelse(type=="Movie" & is.na(episodes), 1, episodes))

# do the same for Music type
anime <- anime %>%
  mutate(episodes = ifelse(type=="Music" & is.na(episodes), 1, episodes))

# do we choose median or mean for ONA? take a look at the graph
ONA <- subset(anime, type=="ONA") %>%
  subset(., !is.na(episodes))
hist(ONA$episodes, labels=TRUE, ylim=c(0,550))
# replace with mean since no outliers, so 7 (6.88 rounded up)
anime <- anime %>%
  mutate(episodes = ifelse(type=="ONA" & is.na(episodes), 7, episodes))

# mean and median both around 2 for OVA so replace NA with 2
anime <- anime %>%
  mutate(episodes = ifelse(type=="OVA" & is.na(episodes), 2, episodes))

# do we choose mean or median for specials? take a look at the graph
Special <- subset(anime, type=="Special") %>%
  subset(., !is.na(episodes))
hist(Special$episodes, labels=TRUE, ylim=c(0,1550))
# replace with median since there are 2 outliers, so 1
anime <- anime %>%
  mutate(episodes = ifelse(type=="Special" & is.na(episodes), 1, episodes))

# mean or median for TV? take a look at the graph
tv <- subset(anime, type=="TV") %>%
  subset(., !is.na(episodes))
hist(tv$episodes, labels=TRUE, ylim=c(0,3550))
# replace with median since there are a few outliers, so 24
anime <- anime %>%
  mutate(episodes = ifelse(type=="TV" & is.na(episodes), 24, episodes))

colSums(is.na(anime))
colSums(anime == '')

# check values make sense for ratings data
hist(rating$rating, labels =TRUE)
# seems like these values are okay

write.csv(clean_anime, file = "clean_anime.csv")
# rating was unchanged 