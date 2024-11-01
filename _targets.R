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
    command = load_responses()
  ),
  tar_target(
    name = interaction_raw,
    command = load_interaction_table()
  ),
  tar_target(
    name = games_file, "data/raw/games_ranked.rds", format = "file"
  ),
  tar_target(
    name = games_raw,
    command = load_games(file = games_file)
  ),
  tar_target(
    responses_summary,
    responses |>
    group_by(name)  |>
    summarize(
      mean = mean(response_rating, na.rm = T),
      sd = sd(response_rating, na.rm = T),
      votes = sum(!is.na(response_rating))
    )
  )
)
