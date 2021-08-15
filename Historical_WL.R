library(tidyverse)
library(httr)
library(jsonlite)
library(formattable)

leagueID = "285194"

base = "http://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/"
mid = "?seasonId="
tail = "?view=mNav&view=mStandings"

url = paste0(base,leagueID, tail)

cookies <- c("swid" = "{602DD3C1-2B51-4024-8F26-C9D549A263BE}",
             "espn_s2" = "AEB8VSwjQi4s5hI5xwcvh1ZIfsi%2BQ0R5JP9dvUQWtrQ3rsqQ%2BG4SgoHaoRdhJH1bXHHXiQIG0Cqnr5SxAS7deysvcuna4T1onGBunx7yu6GhUZtSzBIrWVsDsPeLeCRDjKNxgMjHSTDbwugRPPLslTX4wipkIgwXB%2FxbkoQu%2FhpHdrgIeN2lpSw8QAbtIKoSedf8qKNTS27M0vmyaq7kDKu7%2BKM%2FJ5FhC8jkf0QRG2%2B%2F56HH1DOc7aEwZeOafZebkvw%3D")

cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")

ESPNGet <- httr::GET(url = url,
                     config = httr::config(cookie = cookie))

ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet[["content"]])
jsonlite::validate(ESPNRaw)
# ESPNRaw <- gsub('\\[\\{', '\\{', ESPNRaw)
# ESPNRaw <- gsub('\\}\\]', '\\}', ESPNRaw)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw, flatten = T)

getnames <- data.frame()
getID <- data.frame()
getSchedule <- data.frame()

for (x in 1:nrow(ESPNFromJSON)) {
  # Get owner names
  year <- data.frame(ESPNFromJSON[[3]][[x]])
  getnames <- rbind(getnames, year)
  
  # Get owner IDs
  year <- data.frame(ESPNFromJSON[[8]][[x]])
  year$year <- ESPNFromJSON[[6]][[x]]
  getID <- rbind(getID, year)
  
  # Get schedule
  year <- data.frame(ESPNFromJSON[[4]][[x]])
  year$year <- ESPNFromJSON[[6]][[x]]
  getSchedule <- rbind(getSchedule, year)
}

getnames <- getnames[, c(3,2,6)] %>% 
  distinct()

getID <- getID[, c(2,7,8)]

getSchedule <- getSchedule %>% 
  left_join(getID, by = c("away.teamId" = "id", "year" = "year")) %>% 
  left_join(getID, by = c("home.teamId" = "id", "year" = "year")) %>% 
  rename(Away.Team = owners.x, Home.Team = owners.y) %>% 
  mutate(Win = if_else(home.totalPoints > away.totalPoints, Home.Team, if_else(home.totalPoints < away.totalPoints, Away.Team, NULL)),
         Loss = if_else(home.totalPoints < away.totalPoints, Home.Team, if_else(home.totalPoints > away.totalPoints, Away.Team, NULL)),
         Tie.1 = if_else(home.totalPoints == away.totalPoints, Home.Team, NULL),
         Tie.2 = if_else(home.totalPoints == away.totalPoints, Away.Team, NULL)) %>% 
  filter(matchupPeriodId <= 13)

wins <- getSchedule %>% 
  group_by(as.character(Win)) %>% 
  summarise(Wins = n())

losses <- getSchedule %>% 
  group_by(as.character(Loss)) %>% 
  summarise(Losses = n())

tie1 <- getSchedule %>% 
  group_by(Tie.1) %>% 
  summarise(Tie = n())

tie2 <- getSchedule %>% 
  group_by(Tie.2) %>% 
  summarise(Tie = n())

seasons <- getID %>% 
  group_by(as.character(owners)) %>% 
  summarise(Seasons = n())

W.L <- wins %>% 
  left_join(losses, by = c("as.character(Win)" = "as.character(Loss)")) %>% 
  left_join(seasons, by = c("as.character(Win)" = "as.character(owners)"))

getnames <- getnames %>% 
  rename(owner = id)

W.L <- W.L %>% 
  rename(owner = `as.character(Win)`)

W.L <- W.L %>% 
  merge(getnames, by = "owner")

W.L$firstName[W.L$firstName == ""] <- "Jared"
W.L$lastName[W.L$lastName == ""] <- "West"

W.L$firstName <- stringr::str_to_title(W.L$firstName)
W.L$lastName <- stringr::str_to_title(W.L$lastName)
W.L$Name <- paste0(W.L$firstName, " ", W.L$lastName)

W.L <- W.L %>% 
  group_by(Name) %>% 
  summarise(Wins = sum(Wins),
            Losses = sum(Losses),
            Seasons = sum(Seasons)) %>% 
  mutate("Win %" = percent(Wins / (Wins + Losses), digits = 2))

write.csv(W.L, file = "historical_WL.csv", row.names = F)

rm(list = ls())