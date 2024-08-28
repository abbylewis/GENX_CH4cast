score_all_forecasts <- function(forecasts, observations) {
  #To speed up scoring, cut out forecasts that don't have an associated observation
  comb <- forecasts %>%
    left_join(observations) %>%
    filter(!is.na(observation)) %>%
    select(-observation)
  
  scores <- score4cast::score(comb, observations)
  write.csv(scores, "scores.csv")
  return(scores)
}