library(tidyverse)
library(spotifyr)
dplyr::bind_rows()

corpus_king <- get_playlist_audio_features("", "37i9dQZF1DXaTIN6XNquoW")
corpus_queen <- get_playlist_audio_features("", "37i9dQZF1DWTQllLRMgY9S")

combined_corpus <-
  bind_rows(
    corpus_king %>% mutate(category = "King of pop (Michael Jackson)"),
    corpus_queen %>% mutate(category = "Queen of pop (Madonna)")
  )

#$name <- rownames(combined_corpus)
combined_corpus %>%
  ggplot(aes(x = energy, y = danceability, color = factor(mode), size = valence)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  geom_rug(size = 0.1) +
  facet_wrap(~category) +
  scale_color_manual(name = "mode", 
                     values = c("lightblue", "steelblue4"), labels = c('Minor', 'Major')) +
  theme_bw()

  #geom_text_repel(data = subset(combined_corpus, energy < 0.25), aes(label = name))


print('test')

