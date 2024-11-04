library(tidyverse)

# non interactive authentication to google cloud storage
auth_gcp = function(json_file = Sys.getenv("GCS_AUTH_FILE"),
                    scopes = c("https://www.googleapis.com/auth/devstorage.full_control", "https://www.googleapis.com/auth/cloud-platform")) {
    
    googleAuthR::gar_auth_service(json_file = Sys.getenv("GCS_AUTH_FILE"), scope = scopes)
    
}

# authenticate to gcp
auth_gcp()

# games
games_all = bggUtils:::get_games_from_gcp()
games_ranked = bggUtils:::get_games_from_gcp(object = "raw/objects/ranked_games")

# save to data/raw
saveRDS(games_ranked, file = "data/raw/games_ranked.rds")
saveRDS(games_all, file = "data/raw/games_all.rds")

reticulate::py_save_object(games_ranked, "data/raw/games_ranked.pkl")

# survey responses
googlesheets4::gs4_auth()

# set url for google sheets
url = "https://docs.google.com/spreadsheets/d/1wBSJGwkJOGaiPINFqda3FESAnSQnTF9y4rD8uMX9liw/edit?gid=531698319#gid=531698319"

# raw responses
responses_raw = googlesheets4::read_sheet(url, sheet = "Form Responses 1")

# summarized responses
responses_table = googlesheets4::read_sheet(url, sheet = "Player Interaction Table (Summary)")

library(tidyverse)
responses_raw |>
    rename(
        response_id = 1,
        response_ts = Timestamp
    ) |>
    pivot_longer(
        cols = -c("response_id", "response_ts"),
        values_to = c("response_rating")
    ) |>
    group_by(name, response_rating) |>
    count() |>
    filter(!is.na(response_rating)) |>
    pivot_wider(
        names_from = c("response_rating"),
        values_from = c("n"),
        values_fill = 0,
        names_prefix = "rating_"
    )
    arrange(rating_NA)
    group_by(name) 
    summarize(
        mean = mean(response_rating, na.rm = T),
        sd = sd(response_rating, na.rm = T),
        votes = sum(!is.na(response_rating))
    ) |>
    arrange(desc(votes))



