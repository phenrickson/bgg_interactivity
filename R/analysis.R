summarize_responses <- function(data, var = response_rating) {
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

# gt table
gt_tbl = function(data) {
  
  data |>
    gt::gt() |>
    gtExtras::gt_theme_espn()
}

# create a table showing number of votes and mean/sd for games
gt_ratings_tbl = function(data) {
  
  data |>
    gt_tbl() |>
    gt::sub_missing() |>
    gt::cols_align(columns = -c("name"), align = "center") |>
    gt::fmt_number(decimals = 3, columns = c("mean", "sd")) |>
    gt::cols_label(
      rating_1 = 1,
      rating_2 = 2,
      rating_3 = 3,
      rating_4 = 4,
      rating_5 = 5
    ) |>
    gt::cols_width(
      starts_with("rating_") ~ px(50),
      mean ~ px(100),
      sd ~ px(100),
      n ~ px(100)
    ) |>
    gt::opt_interactive() |>
    gt::data_color(columns = c("mean"),
                   method = "numeric",
                   palette = "viridis",
                   domain = c(1, 5),
                   reverse = T)
  
}

uncount_responses = function(data) {
  
  data |>
    select(name, starts_with("rating_")) |>
    pivot_longer(cols = starts_with("rating_"),
                 names_to = c("rating"),
                 names_prefix = "rating_",
                 values_to = "value") |>
    group_by(name, rating) |>
    uncount(value) |>
    mutate(response_rating = as.numeric(rating)) |>
    ungroup() |>
    select(name, response_rating)
  
}

summarize_bootstraps = function(data) {
  
  
  data |>
    mutate(boot = map(splits, analysis)) |>
    mutate(summary = map(boot, 
                         ~ .x |> 
                           group_by(name) |> 
                           summarize_responses(var = response_rating))) |>
    select(id, summary) |>
    unnest(summary) |>
    select(id, name, est = mean)
  
}

add_rating_labels = function(data) {
  
  set_rating_labels = function(var) {
    
    factor(var, 
           labels = c("1 Low",
                      "2 Low-Moderate",
                      "3 Moderate",
                      "4 Moderate-High",
                      "5 High")
    )
    
  }
  
  data |>
    mutate(rating = set_rating_labels(rating))
}

wider_responses = function(data) {
  
  data |>
    pivot_wider(names_from = c("rating"), values_from = c("n"), values_fill = 0, names_prefix = "rating_")
}

longer_responses = function(data) {
  
  data |>
    pivot_longer(cols = starts_with("rating"),
                 names_to = c("rating"),
                 names_prefix = "rating_")
}


add_rank_facet = function(data) {
  
  data |>
    mutate(rank = rank(-mean),
           rank_cut = cut(rank, breaks = 2, labels = F)) |>
    group_by(rank_cut) |>
    mutate(rank_facet = paste(min(rank), max(rank), sep = "-"))
  
}

add_rating_labels = function(data) {
  
  set_rating_labels = function(var) {
    
    factor(var, 
           labels = c("1 Low",
                      "2 Low-Moderate",
                      "3 Moderate",
                      "4 Moderate-High",
                      "5 High")
    )
    
  }
  
  data |>
    mutate(rating = set_rating_labels(rating))
}


plot_responses_count = function(data) {
  
  df = 
    data |>
    add_rank_facet() |>
    longer_responses() |>
    add_rating_labels()
  
  df |>
    ggplot(aes(y=reorder(name, mean), x=value, fill = rating))+
    geom_col(position = 'fill')+
    scale_fill_viridis_d(begin = 1, end = 0)+
    facet_wrap(rank_facet ~., scales = "free_y")+
    ylab("")+
    xlab("proportion of ratings")+
    theme(axis.text.y = element_text(size = 6),
          legend.title = element_text()) +
    guides(fill = guide_legend(title = 'Player Interactivity',
                               title.position = 'top',
                               title.hjust = 0.5))
  
}

format_as_table = function(data) {
  
  data |>
    group_by(name) |>
    summarize_responses() |>
    inner_join(
      data |>
        count_ratings() |>
        wider_responses()
    ) |>
    select(name, starts_with('rating_'), n, mean, sd)
  
}

plot_mean_vs_sd = function(data) {
  
  data |>
    ggplot(aes(x=mean, y=sd, label = name))+
    geom_point(aes(size = n), position = position_auto(), alpha = 0.5)+
    geom_text(check_overlap = T, size = 1.5, vjust = -1, position = position_auto())
}
