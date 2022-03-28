library(tidyverse)
library(spotifyr)
library(forcats)
library(plotly)
library(flexdashboard)
library(compmus)
library(reshape)
library(tidymodels)
library(ggdendro)
library(heatmaply)


corpus_king <- get_playlist_audio_features("", "37i9dQZF1DXaTIN6XNquoW")

corpus_king_adapted <- corpus_king[c(-50),]

corpus_queen <- get_playlist_audio_features("", "37i9dQZF1DWTQllLRMgY9S")

corpus_king_small <- head(corpus_king, 10)
corpus_queen_small <- head(corpus_queen, 10)

best_track_king_billie <- get_track_audio_features(c("2IU9ftOgyRL2caQGWK1jjX", "5ChkMS8OtdzJeqyybCc9R5"))
best_track_queen_vergin <- get_track_audio_features(c("1C2h7mLntPSeVYciMRTF4a", "1ZPlNanZsJSPK5h9YZZFbZ"))

combined_corpus_small <-
  bind_rows(
    corpus_king_small %>% mutate(category = "King of pop (Michael Jackson)"),
    corpus_queen_small %>% mutate(category = "Queen of pop (Madonna)")
  )

  
get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}  

test <-
  corpus_king %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

test_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B,
    data = test
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(test %>% mutate(track.name = str_trunc(track.name, 40))) %>%
  juice() %>%
  column_to_rownames("track.name")

halloween_dist <- dist(test_juice, method = "euclidean")

halloween_dist %>% 
  hclust(method = "single") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram()


heatmaply(
  test_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)













indie_features <-
  combined_corpus %>%  # For your portfolio, change this to the name of your corpus.
  add_audio_analysis() %>% 
  mutate(
    playlist = factor(artist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))


indie_recipe <-
  recipe(
    playlist ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B,
    data = indie_features,          # Use the same name as the previous block.
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].


indie_cv <- indie_features %>% vfold_cv(5)


knn_model <-
  nearest_neighbor(neighbors = 1) %>%
  set_mode("classification") %>% 
  set_engine("kknn")
indie_knn <- 
  workflow() %>% 
  add_recipe(indie_recipe) %>% 
  add_model(knn_model) %>% 
  fit_resamples(
    indie_cv, 
    control = control_resamples(save_pred = TRUE)
  )


indie_knn %>% get_conf_mat()


indie_knn %>% get_conf_mat() %>% autoplot(type = "mosaic")

indie_knn %>% get_conf_mat() %>% autoplot(type = "heatmap")


indie_knn %>% get_pr()


forest_model <-
  rand_forest() %>%
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity")
indie_forest <- 
  workflow() %>% 
  add_recipe(indie_recipe) %>% 
  add_model(forest_model) %>% 
  fit_resamples(
    indie_cv, 
    control = control_resamples(save_pred = TRUE)
  )

indie_forest %>% get_pr()


workflow() %>% 
  add_recipe(indie_recipe) %>% 
  add_model(forest_model) %>% 
  fit(indie_features) %>% 
  pluck("fit", "fit", "fit") %>%
  ranger::importance() %>% 
  enframe() %>% 
  mutate(name = fct_reorder(name, value)) %>% 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")


indie_features %>%
  ggplot(aes(x = acousticness, y = c01, colour = playlist, size = energy)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "dancebility",
    y = "co6",
    size = "Energy",
    colour = "Playlist"
  )
