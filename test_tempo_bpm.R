library(tidyverse)
library(spotifyr)
library(forcats)
library(plotly)
library(flexdashboard)
library(compmus)
library(reshape)

combined_corpus$artist <-  sapply(combined_corpus$track.album.artists, function(mat) unlist(mat)[3])

combined_corpus %>%
  ggplot(aes(x <- tempo)) + geom_density()


print(combined_corpus %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo))

print(combined_corpus %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo))

abc <- get_tidy_audio_analysis("2VCUcIPY3i6pv9Nu9Vg6k3") %>%
  select(segments) %>%
  unnest(segments)

abc %>%
  mutate(loudness_max_time = start + loudness_max_time) %>%
  arrange(loudness_max_time) %>%
  mutate(delta_loudness = loudness_max - lag(loudness_max)) %>%
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")


abc %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  arrange(start) %>%
  mutate(pitches = map2(pitches, lag(pitches), `-`)) %>%
  slice(-1) %>% 
  compmus_gather_chroma() %>% 
  group_by(start, duration) %>% 
  summarise(novelty = sum(log1p(pmax(value, 0)))) %>% 
  ggplot(aes(x = start + duration / 2, y = novelty)) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")




cor(combined_corpus[,6:16]) %>% melt() -> melt_playlist
melt_playlist

melt_playlist$value[melt_playlist$value < 0.1 & melt_playlist$value > -0.1] = NA #subset the small correlations and make them NA

#Visualize correlations
melt_playlist %>% 
  ggplot(aes(x=X1, y=X2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Var 1") +
  ylab("Var 2") +
  labs(title = "Correlations between Spotify audio features in complete corpus")
