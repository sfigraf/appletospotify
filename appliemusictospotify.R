library(tidyverse)
library(stringr)
library(data.table)
###Script for converting apple music playlist to csv with songs and artists
#if you don't have an apple music account but do have a spotify one
#made for a csv with one column: the copied and pasted songs, artists, and times 
#for now they're all in one column
data <- read.csv(file.choose())
View(data)
colnames(data)[1] <- "Text"
x <- data %>%
  filter(str_detect(data$Text,
                    "[:alpha:]")) 
x <- x %>%
  mutate(row = as.numeric(rownames(x)))


x <- x %>%
  mutate(song = ifelse(row %% 2 == 1,
                       as.character(Text),
                       NA)) %>%
  mutate(artist = ifelse(row %% 2 == 0,
                as.character(Text),
                NA)) 
artists <- x %>%
  filter(is.na(x$song)) %>%
  select(artist)
songs<- x %>%
  filter(is.na(x$artist)) %>%
  select(song)

#joining the datasets
songs_artists1 <- songs %>%
  mutate(artists = artists$artist)

songs_artists1 %>%
  write.csv("woodhouseplaylist.csv")

#use this url to convert csv to spotify 
#https://www.tunemymusic.com/CSV-to-Spotify.php#step1
