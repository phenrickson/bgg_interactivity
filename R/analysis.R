summarize_responses <- function(data, var) {
  data |>
  summarize(
    mean = mean({{ var }}, na.rm = TRUE),
    sd = sd({{ var }}, na.rm = TRUE),
    n = sum(!is.na({{ var }}))
  )
}

count_ratings = function(data) {
  
  data |>
  filter(!is.na(response_rating)) |>
  group_by(name, rating = response_rating) |>
  count() |>
  ungroup()
  
}
