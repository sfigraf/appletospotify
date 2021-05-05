###Script for converting apple music playlist to csv with songs and artists
#if you don't have an apple music account but do have a spotify one
#previous version was made for a csv input that is initally made by copying and pasting apple music songs from the website
#but apple has made it so you can't copy and paste song title from their site
#now it webscrapes for songs, artists and album
#not 100% effetive, not least of all becuase apple only shows the first 100 songs on their main playlist screen
#haven't figured out how to get around that yet so for now it only works on the first 100 songs
# things would be a lot easier probably if i just got a apple music account but fuck that

#also the songs listed on apple music are not always 100% the same as on spotify so there is some mismatch there
#data <- read.csv(file.choose())

library(rvest)
library(tidyverse)
library(data.table)
url <- "https://music.apple.com/us/playlist/lets-run/pl.u-KVXBBv6CAAaEd"

#find tags by viewing page source
#can specify a class with . so this only gets 'class songs-list-row__song-name'

# Webscraping -------------------------------------------------------------

#song names
music<- url %>%
  read_html() %>%
   #the period means you only get this specific class type within the div node
  html_nodes("div.songs-list-row__song-name") %>% 
  html_text() #because we're scraping text
#html_table(fill = TRUE)
song_names <- data.frame(music)

#artists and albums: differnet html nodes
music2 <- url %>%
  read_html() %>%
  html_nodes("div span") %>% ##this is the selector to find what text we need; span a gets some song names and artists; div span gets artist names in same entry
  html_text()

artists_and_albums <- data.frame(music2)
View(artists_and_albums)


# Data Wrangling ----------------------------------------------------------


colnames(artists_and_albums)[1] <- "Text"
# x1 <- data %>%
#   filter(str_detect(data$Text,
#                     "[:alpha:]")) #[:alpha:]
x1 <- artists_and_albums

#cleaning up data: might need to adjust this based off different playlists
x1 <- x1 %>% 
  filter(row_number() <= n()-2) %>% #takes off last 2 rows
  slice(-(1:9)) #takes off first 9 rows

#gtes column of rownumbers
x2<- x1 %>%
  mutate(row = as.numeric(rownames(x1)))
#separate takes a column and malkes it into multiple based off delimiters in column
#?separate
# x2 <- x1 %>%
#   separate(Text, into =  c("artist", "album"), extra = "merge")

#separates text into new columns
x2<- x2%>%
  mutate(artist = ifelse(row %% 2 == 1,
                       as.character(Text),
                       NA)) %>%
  mutate(album = ifelse(row %% 2 == 0,
                as.character(Text),
                NA)) 
#fills columns down and deselcts unneeded columns
x3 <- x2%>%
  fill(artist) %>%
  na.omit() %>%
  select(-c(Text, row))

# Final Dataset -----------------------------------------------------------


full_dataset <- bind_cols(song_names, x3)
#gets rid of whitespace in entries
full_dataset <- full_dataset %>%
  mutate("artist2" = str_squish(artist),
         "album2" = str_squish(album)) %>%
  rename("Song" = music) %>%
  select(c(Song,artist2,album2))
  

#saving csv
full_dataset %>%
  write.csv("philworkoutplaylist1.csv")

#use this url to convert csv to spotify 
#https://www.tunemymusic.com/CSV-to-Spotify.php#step1


