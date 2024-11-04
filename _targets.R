# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(qs)
# library(tarchetypes) # Load other packages as needed.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Set target options:
tar_option_set(
  packages = c("tidyverse"), # Packages that your targets need for their tasks.
  format = "qs", # Optionally set the default storage format. qs is fast.
)

# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = responses,
    load_responses()
  ),
  tar_target(
    name = interaction_raw,
    load_interaction_table()
  ),
  tar_target(
    name = interaction_table,
    interaction_raw |>
      tidy_interaction_table()
  ),
  tar_target(
    name = games_file, 
    "data/raw/games_ranked.rds", 
    format = "file"
  ),
  tar_target(
    name = games_raw,
    load_games(file = games_file)
  ),
  tar_target(
    name = games_preprocessed,
    games_raw |> bggUtils::preprocess_bgg_games()
  ),
  tar_target(
    interaction_estimates,
    interaction_table |>
      select(name, interaction = mean) |>
      mutate(
        name = case_when(name == 'Welcome To…' ~ "Welcome To...",
                         name == 'Railroad Ink' ~ 'Railroad Ink: Deep Blue Edition',
                         TRUE ~ name)
      )
  )
)
