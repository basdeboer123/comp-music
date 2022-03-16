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


abc_2 <- get_tidy_audio_analysis("2VCUcIPY3i6pv9Nu9Vg6k3")


abc_2 %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

gc()

cor(combined_corpus[,6:16]) %>% melt() -> mept_combined
melt_combined$value 

melt_playlist %>%
  ggplot(aes(x = Var1, y = Var2, fill = value))
