recipe_linear = function(data,
                         outcome,
                         ids = id_vars(),
                         predictors = predictor_vars()) {
  
  data |>
    build_recipe(
      outcome = {{outcome}},
      ids = ids,
      predictors = predictors
    ) |>
    add_bgg_preprocessing() |>
    add_linear_preprocessing()   
  
}

glmnet_spec = function() {
  
  logistic_reg(penalty = tune::tune(),
               mixture = tune::tune()) %>%
    set_engine("glmnet")
}

glmnet_grid = function(penalty = seq(-3, -0.75, length = 10),
                       mixture = c(0)) {
  
  expand.grid(
    penalty = 10 ^ penalty,
    mixture = mixture
  )
}


# recipes for bgg outcomes
predictor_vars = function(vars =
                            c("minplayers",
                              "maxplayers",
                              "playingtime",
                              "minplaytime",
                              "maxplaytime",
                              "image",
                              "thumbnail",
                              "minage",
                              "categories",
                              "mechanics",
                              "publishers",
                              "designers",
                              "artists",
                              "families",
                              "mechanisms",
                              "components",
                              "themes"
                            )
) {vars}

id_vars = function(vars =
                     c("game_id", "name", "yearpublished")) 
  {vars}

# spline_vars = function(vars = c("number_mechanics",
#                                 "number_categories")) {vars}
# 
# discrete_vars = function(vars = spline_vars()) {vars}
# 
# 
# # basic recipe setup
build_recipe = function(data,
                        outcome,
                        ids = id_vars(),
                        predictors = predictor_vars(),
                        ...) {
  
  recipe(x=data) %>%
    # set ids
    update_role(
      any_of(ids),
      new_role = "id"
    ) %>%
    # set predictors
    update_role(
      any_of(predictors),
      new_role = "predictor"
    ) %>%
    # set outcome
    update_role(
      {{ outcome }},
      new_role = "outcome"
    ) %>%
    # set anything else as id
    update_role(
      -has_role("predictor"),
      -has_role("outcome"),
      -has_role("id"),
      new_role = "extras"
    ) |>
    step_rm(has_role("extras"))
}

# create resamples
create_resamples = function(data,
                            v,
                            strata = own) {
  
  data |>
    vfold_cv(
      v = v,
      strata = {{strata}}
    )
}

# standard preprocessing for bgg variables
add_bgg_preprocessing = function(recipe,
                                 ...) {
  
  recipe |>
    step_rm(has_role("extras")) |>
    add_preprocessing() |>
    add_imputation() |>
    add_bgg_dummies(...)
}

add_linear_preprocessing = function(recipe) {
  
  recipe |>
    # spline for year
    add_splines(vars = "year", degree = 4) |>
    # splines with fifth degree polynomials for mechanics/categories
    add_splines(c("number_mechanics", "number_categories")) |>
    # remove zero variance
    add_zv() |>
    # filter for correlation
    step_corr(all_numeric_predictors(), threshold = 0.95) |>
    add_normalize()
}

# function for extracting dummies from nominal features
add_dummies = function(recipe,
                       variable,
                       threshold = 100) {
  
  variable = enquo(variable)
  
  recipe %>%
    # tokenize
    step_dummy_extract(!!variable,
                       sep = ", ",
                       other = "remove_other_field",
                       threshold = threshold) %>%
    # remove other var
    step_rm(contains("remove_other_field"))
  
  
}

# standard dummy recipes
add_bgg_dummies = function(recipe,
                           mechanics_threshold = 1,
                           categories_threshold = 1,
                           families_threshold = 100,
                           publishers_threshold = 1,
                           designers_threshold = 10,
                           artists_threshold = 10,
                           components_threshold = 25,
                           themes_threshold = 25,
                           mechanisms_threshold = 25
) {
  
  recipe %>%
    # include most mechanics
    add_dummies(mechanics,
                threshold = mechanics_threshold) %>%
    # include all categories
    add_dummies(categories,
                threshold = categories_threshold) %>%
    # families
    add_dummies(families,
                threshold = families_threshold) %>%
    # publishers
    add_dummies(publishers,
                threshold = publishers_threshold) %>%
    # designers
    add_dummies(designers,
                threshold = designers_threshold) %>%
    # artists
    add_dummies(artists,
                threshold = artists_threshold) %>%
    # components
    add_dummies(components,
                threshold = components_threshold) %>%
    # themes
    add_dummies(themes,
                threshold = themes_threshold) %>%
    # mechanisms
    add_dummies(mechanisms,
                threshold = mechanisms_threshold)
}

step_playingtime_and_playercount = function(recipe) {
  
  recipe |>
    # truncate playercounts
    step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                              TRUE ~ minplayers)) |>
    step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                              maxplayers <=1 ~ 1,
                                              TRUE ~ maxplayers)) |>
    # add time per player
    step_mutate(time_per_player = playingtime/ maxplayers) |>
    # impute missingness in selected features with median
    step_impute_median(playingtime,
                       minplayers,
                       maxplayers,
                       minage,
                       time_per_player) |>
    # log
    step_log(time_per_player,
             playingtime,
             offset = 1) |>
    step_rm(minplaytime, maxplaytime)
}

# try dummy for playercounts?
step_dummy_playercounts = function(recipe, threshold = 1) {
  
  recipe |>
    step_mutate(player_counts = map2_vec(minplayers, maxplayers, ~ paste(seq(.x, .y), collapse = ","))) |>
    step_dummy_extract(player_counts,
                       sep = ",",
                       other = "other",
                       threshold = threshold) |>
    step_rm(minplayers, maxplayers)
  
}

step_count_items <- function(recipe, ..., prefix = "number") {
  terms <- enquos(...)
  
  for (term in terms) {
    var_name <- paste0(prefix, "_", quo_name(term))
    
    recipe <- recipe %>%
      step_mutate(!!var_name := case_when(
        is.na(!!term) ~ 0,                             # If NA, set to 0
        !!term == "" ~ 0,                              # If empty, set to 0
        TRUE ~ str_count(!!term, ",") + 1              # Count commas + 1
      ))
  }
  
  recipe
}


# standardized preprocessing
add_preprocessing = function(recipe) {
  
  recipe %>%
    # make time per player variable
    step_mutate(time_per_player = playingtime/ maxplayers) %>%
    # remove zero variance predictors
    step_zv(all_predictors()) %>%
    # number_mechanics
    step_mutate(number_mechanics =
                  dplyr::case_when(
                    is.na(mechanics) ~ 0,
                    TRUE ~ stringr::str_count(mechanics, ',') + 1
                  )
    ) %>%
    # number categories
    step_mutate(number_categories =
                  dplyr::case_when(
                    is.na(categories) ~ 0,
                    TRUE ~ stringr::str_count(categories, ',') + 1
                  )
    ) %>%
    # log time per player and playingtime
    step_log(time_per_player,
             playingtime,
             offset = 1) %>%
    # truncate yearpublished
    step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                        TRUE ~ yearpublished),
                role = "predictor") %>%
    # indicator for published before 1900
    step_mutate(published_before_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                         TRUE ~ 0)) %>%
    # solo game
    # big box/deluxe/anniversary edition
    step_mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                  TRUE ~ 0)) %>%
    # description word count
    step_mutate(word_count = stringi::stri_count_words(description)) %>%
    step_mutate(word_count = tidyr::replace_na(word_count, 0))
}


# imputation
add_imputation = function(recipe) {
  
  recipe %>%
    # impute missingness in selected features with median
    step_impute_median(playingtime,
                       minplayers,
                       maxplayers,
                       minage,
                       time_per_player) %>% # medianimpute numeric predictors
    # truncate minage to no greater than 18
    step_mutate(minage = dplyr::case_when(minage > 18 ~ 18,
                                          minage < 0 ~ 0,
                                          TRUE ~ minage)) %>%
    # truncate player counts
    step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                              TRUE ~ minplayers)) %>%
    step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                              maxplayers <=1 ~ 1,
                                              TRUE ~ maxplayers)) %>%
    step_rm(minplaytime, maxplaytime)
}


# normalize all numeric predictors
add_normalize = function(recipe) {
  
  recipe %>%
    step_normalize(all_numeric_predictors())
}
# 

# step to remove zero variance
add_zv = function(recipe) {
  
  recipe %>%
    step_zv(all_numeric_predictors())
  
}

# splines
# add splines for nonlinear effects for linear models
add_splines= function(recipe,
                      vars = c("year",
                               "number_mechanics",
                               "number_categories"),
                      degree = 5) {
  
  
  step_spline_feature = function(recipe,
                                 feature,
                                 ...) {
    
    feature = enquo(feature)
    
    recipe %>%
      # add splines
      step_ns(
        !!feature,
        deg_free = degree)
  }
  
  for (i in 1 :length(vars)) {
    
    recipe = recipe %>%
      step_spline_feature(feature = !!vars[i])
    
  }
  
  recipe
  
  
}