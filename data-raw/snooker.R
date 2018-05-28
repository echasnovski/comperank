library(tidyverse)

# devtools::install.github("echasnovski/snookerorg")
# Version, used to create actual data, is 0.1.0 as of 2018-05-28
library(snookerorg)

season_vec <- 2016:2017


# Preprocessing functions -------------------------------------------------
# Replace common non-ASCII characters with ASCII
replace_special_chr <- function(x) {
  x %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ß", "ss")
}

# Remove extra whitespace and replace common non-ASCII characters with ASCII
handle_tbl_strings <- function(tbl) {
  tbl %>%
    mutate_if(is.character, . %>% str_trim() %>% replace_special_chr)
}


# Fetch events ------------------------------------------------------------
raw_events <- map(season_vec, snookerorg::get_season_events) %>%
  bind_rows() %>%
  as_tibble()
saveRDS(raw_events, file.path("data-raw", "raw_events.rds"))

snooker_events <- raw_events %>%
  filter(
    type %in% c("Invitational", "Qualifying", "Ranking"),
    discipline == "snooker", # Not Six Red or Power snooker
    sex == "Both",
    # "CVB Snooker Challenge" (id == 663) is a team event
    id != 663L,
    # "World Cup" (id == 626) is a team event with 4 group stages stored as
    # separate events (other identifiers)
    !(id %in% c(626L, 658L, 659L, 660L, 661L)),
    # For some reason "Womens British Open" groups are stored
    # with `sex` equals "both" but they are female
    !grepl("women.*british.*open", name, ignore.case = TRUE),
    ageGroup == "O",
    # "Shoot-Out"s should be removed as scores represent scores in one frame
    !grepl("shoot-out", name, ignore.case = TRUE)
  ) %>%
  snookerorg::simplify_event() %>%
  select(-discipline, -sex, -ageGroup) %>%
  select(id, season, name, everything()) %>%
  handle_tbl_strings() %>%
  arrange(startDate)


# Fetch players -----------------------------------------------------------
# `get_event_players()` doesn't return correct "status" column
# This is why multiple stage fetching is needed

# List of professionals is taken as for last season
pro_players <- snookerorg::get_season_pro_players(tail(season_vec, n = 1))

# Amateurs as for last season are players that were actually amateurs that
# played tournaments or professionals that dropped out of main tour before last
# season
ama_players_expro <- map(
  head(season_vec, -1), snookerorg::get_season_pro_players
) %>%
  bind_rows() %>%
  filter(!duplicated(id)) %>%
  anti_join(y = pro_players, by = "id") %>%
  mutate(status = "ama")

ama_players_actual <- map(season_vec, snookerorg::get_season_ama_players) %>%
  bind_rows() %>%
  filter(!duplicated(id))

ama_players <- bind_rows(ama_players_expro, ama_players_actual) %>%
  filter(!duplicated(id))

# Only players actually played in events is of interest
event_players <- map(snooker_events$id, function(id) {
  Sys.sleep(1)

  snookerorg::get_event_players(id)
}) %>%
  bind_rows() %>%
  filter(!duplicated(id))
saveRDS(event_players, file.path("data-raw", "event_players.rds"))

# Constructing snooker players data
snooker_players <- bind_rows(pro_players, ama_players) %>%
  # If player is described as both "pro" and "ama" he should have "pro" status
  # as it is taken for season 2017/2018 manually
  filter(!duplicated(id)) %>%
  semi_join(y = event_players, by = "id") %>%
  as_tibble() %>%
  arrange(id) %>%
  mutate(
    name = if_else(
      surnameFirst,
      str_squish(paste(lastName, middleName, firstName)),
      str_squish(paste(firstName, middleName, lastName))
    )
  ) %>%
  select(id, name, nationality, sex, born, status) %>%
  handle_tbl_strings()


# Fetch matches -----------------------------------------------------------
raw_matches <- map(snooker_events$id, function(id) {
  Sys.sleep(1)

  snookerorg::get_event_matches(id)
}) %>%
  bind_rows() %>%
  as_tibble()
saveRDS(raw_matches, file.path("data-raw", "raw_matches.rds"))

snooker_matches <- raw_matches %>%
  snookerorg::simplify_match() %>%
  arrange(startDate) %>%
  select(-number) %>%
  handle_tbl_strings()


# Sanity checks -----------------------------------------------------------
# Check uniqueness of `id` column. The result should be TRUE
isTRUE(sum(duplicated(snooker_players$id)) == 0)

# All players in snooker_matches are present in snooker_players and vice versa.
# Should both be of lengths zero
setdiff(
  unique(c(snooker_matches$player1Id, snooker_matches$player2Id)),
  snooker_players$id
)
setdiff(
  snooker_players$id,
  unique(c(snooker_matches$player1Id, snooker_matches$player2Id))
)

# `winnerId` in `snooker_matches` is taken from either `player1Id` of
# `player2Id`. Result should be 0.
sum(
  (snooker_matches$winnerId != snooker_matches$player1Id) &
    (snooker_matches$winnerId != snooker_matches$player2Id)
)

# Check that there are no matches with two walkovers. Result should be 0.
sum(snooker_matches$walkover1 & snooker_matches$walkover2)

# Detect non-ASCII characters. Result should be zero in all cases.
detect_non_ascii <- . %>% transmute_if(is.character, stringi::stri_enc_mark) %>%
  mutate(id = 1:n()) %>%
  gather(var, value, -id) %>%
  summarise(numNonASCII = sum(value != "ASCII", na.rm = TRUE))
detect_non_ascii(snooker_events)
detect_non_ascii(snooker_players)
detect_non_ascii(snooker_matches)

# Check that all players have names to (weakly) avoid team events.
# Both results should be 0.
sum(is.na(snooker_players$name))
sum(snooker_players$name == "", na.rm = TRUE)

# Check that all events have at least one pro played in at least one match.
# Result should have 0 rows.
snooker_matches %>%
  left_join(
    y = snooker_players %>% select(id, status1 = status),
    by = c(player1Id = "id")
  ) %>%
  left_join(
    y = snooker_players %>% select(id, status2 = status),
    by = c(player2Id = "id")
  ) %>%
  select(eventId, status1, status2) %>%
  gather(status, value, -eventId) %>%
  group_by(eventId) %>%
  summarise(anyPro = any(value == "pro")) %>%
  filter(!anyPro)


# File work ---------------------------------------------------------------
save(
  snooker_events, file = file.path("data", "snooker_events.rda"),
  compress = "bzip2"
)
save(
  snooker_players, file = file.path("data", "snooker_players.rda"),
  compress = "bzip2"
)
save(
  snooker_matches, file = file.path("data", "snooker_matches.rda"),
  compress = "bzip2"
)
file.remove(
  file.path(
    "data-raw", paste0(c("event_players", "raw_events", "raw_matches"), ".rds")
  )
)
