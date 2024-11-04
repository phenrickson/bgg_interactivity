# auth to googlesheets
auth_gs = function(email = Sys.getenv("GARGLE_EMAIL")) {
  
  googlesheets4::gs4_auth(email = Sys.getenv("GARGLE_EMAIL"))
}

# non interactive authentication to google cloud storage
auth_gcp = function(json_file = Sys.getenv("GCS_AUTH_FILE"),
                    scopes = c("https://www.googleapis.com/auth/devstorage.full_control", "https://www.googleapis.com/auth/cloud-platform")) {
  
  googleAuthR::gar_auth_service(json_file = Sys.getenv("GCS_AUTH_FILE"), scope = scopes)
  
}

# set url for google sheets
googlesheets_url = function() {
  
  "https://docs.google.com/spreadsheets/d/1wBSJGwkJOGaiPINFqda3FESAnSQnTF9y4rD8uMX9liw/edit?gid=531698319#gid=531698319"
  
}

# load responses form from Google Sheets
load_responses = function(url = googlesheets_url(), sheet = "Form Responses 1") {
  
  # authenticate
  auth_gs()
  
  raw = googlesheets4::read_sheet(ss = url, sheet = sheet)
  
  prep_responses_internal = function(data) {
    
    data |>
    dplyr::arrange(Timestamp) |>
    dplyr::rename(
      response_id = 1,
      response_ts = Timestamp
    ) |>
    tidyr::pivot_longer(
      cols = -c("response_id", "response_ts"),
      values_to = c("response_rating")
    )
  }

  raw |>
    prep_responses_internal()
  
}

# load summary table of responses from Google Sheets
load_interaction_table = function(url = googlesheets_url(), sheet = "Player Interaction Table (Summary)") {
  
  # authenticate
  auth_gs()
  
  # load
  raw = googlesheets4::read_sheet(url, sheet = "Player Interaction Table (Summary)")
  raw
}

# currently have to hard code some of this to extract the pieces I need
tidy_interaction_table = function(data) {
  
  data |> 
    janitor::clean_names() |> 
    dplyr::select(name = game, 
                  x14:x18, 
                  n=votes, 
                  mean, 
                  sd = std) |>
    dplyr::rename(rating_1 = x14,
                  rating_2 = x15,
                  rating_3 = x16,
                  rating_4 = x17,
                  rating_5 = x18) |>
    dplyr::select(name, starts_with("rating"), n, mean, sd)
}

# load games
load_games = function(file = "data/raw/games_ranked.rds") {
  
  readRDS(file = file)
}
