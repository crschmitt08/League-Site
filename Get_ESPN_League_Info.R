library(tidyverse)
library(httr)
library(jsonlite)
library(formattable)
library(ggrepel)

year = "2020"
leagueID = "285194"

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
mid = "/segments/0/leagues/"
tail = "?view=mDraftDetail&view=mSettings"

url = paste0(base,year,mid,leagueID,tail)

cookies <- c("swid" = "{602DD3C1-2B51-4024-8F26-C9D549A263BE}",
             "espn_s2" = "AEB8VSwjQi4s5hI5xwcvh1ZIfsi%2BQ0R5JP9dvUQWtrQ3rsqQ%2BG4SgoHaoRdhJH1bXHHXiQIG0Cqnr5SxAS7deysvcuna4T1onGBunx7yu6GhUZtSzBIrWVsDsPeLeCRDjKNxgMjHSTDbwugRPPLslTX4wipkIgwXB%2FxbkoQu%2FhpHdrgIeN2lpSw8QAbtIKoSedf8qKNTS27M0vmyaq7kDKu7%2BKM%2FJ5FhC8jkf0QRG2%2B%2F56HH1DOc7aEwZeOafZebkvw%3D")

cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")

ESPNGet <- httr::GET(url = url,
                     config = httr::config(cookie = cookie))

ESPNRaw <- rawToChar(ESPNGet[["content"]])
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

per_id <- as.integer(ESPNFromJSON$scoringPeriodId) - 5

get_data <- function(leagueID = leagueID, per_id = per_id){
  base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
  year = "2020"
  mid = "/segments/0/leagues/"
  tail = str_c("?view=mDraftDetail",
               "&view=mLiveScoring",
               "&view=mMatchupScore,",
               "&view=mPendingTransactions",
               "&view=mPositionalRatings",
               "&view=mRoster",
               "&view=mSettings",
               "&view=mTeam",
               "&view=modular",
               "&view=mNav",
               "&view=mMatchupScore",
               paste0("&scoringPeriodId=",per_id))


url = paste0(base,year,mid,leagueID,tail)

ESPNGet <- httr::GET(url = url,
                     config = httr::config(cookie = cookie))
ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

# roster_size <- 16
number_of_teams <- length(ESPNFromJSON$teams$id)
team_ids <- ESPNFromJSON$teams$id

## one players stats
player_extract <- function(team_number = 1, player_number = 1, per_id = per_id){
  player_week <-
    tibble(
      team = str_c(ESPNFromJSON$teams$location[team_number]," ",ESPNFromJSON$teams$nickname[team_number]),
      teamId = ESPNFromJSON$teams$id[team_number],
      fullName = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$fullName[player_number],
      appliedTotal = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$appliedTotal,
      seasonId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$seasonId,
      scoringPeriodId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$scoringPeriodId,
      statsplitTypeId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$statSplitTypeId,
      externalId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$externalId,
      lineupSlot_id = ESPNFromJSON$teams$roster$entries[[team_number]]$lineupSlotId[player_number],
      eligibleSlots = list(ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$eligibleSlots[[player_number]])
    ) %>% 
    dplyr::filter(seasonId==2020) %>% 
    dplyr::filter(scoringPeriodId != 0) %>% 
    dplyr::filter(scoringPeriodId == per_id)
  return(player_week)
}

schedule1 <- tibble(
  Week = ESPNFromJSON$schedule$matchupPeriodId,
  AwayID = ESPNFromJSON$schedule$away$teamId,
  AwayPoints = ESPNFromJSON$schedule$away$totalPoints,
  HomeID =ESPNFromJSON$schedule$home$teamId,
  HomePoints = ESPNFromJSON$schedule$home$totalPoints,
  Winner =ESPNFromJSON$schedule$winner) %>%
  left_join(
    tibble(
      id = ESPNFromJSON$teams$id,
      AwayTeam = paste(ESPNFromJSON$teams$location,ESPNFromJSON$teams$nickname)
    ), by = c("AwayID"="id")) %>%
  left_join(
    tibble(
      id = ESPNFromJSON$teams$id,
      HomeTeam = paste(ESPNFromJSON$teams$location,ESPNFromJSON$teams$nickname)
    ), by = c("HomeID"="id")
  )

schedule <-
  tibble(
    home = ESPNFromJSON$schedule$away$teamId,
    away = ESPNFromJSON$schedule$home$teamId,
    scoringPeriodId = ESPNFromJSON$schedule$matchupPeriodId,
    gameId = ESPNFromJSON$schedule$id
  ) %>% 
  pivot_longer(cols = c(home,away), values_to = "teamId")
# ESPNFromJSON$schedule$away$totalPoints

#Get the roster slots for each team
get_roster_slots <- function(team_number=1){
  return(tibble(team_number = team_number, player_slot = 1:length(ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats)))
}

team_player_slots <- purrr::map_dfr(1:number_of_teams,~get_roster_slots(team_number = .x))
team_player_slots %>% count(team_number)

team_list <- purrr::map2_dfr(team_player_slots$team_number,team_player_slots$player_slot, ~player_extract(team_number = .x,player_number = .y, per_id = per_id)) %>% 
  left_join(schedule) %>% 
  dplyr::mutate(points_type = if_else(str_length(externalId) > 6, "actual", "projected"))


return(list(team_list = team_list, ESPNFromJSON = ESPNFromJSON, schedule1 = schedule1))

}

team_list <- get_data(leagueID = leagueID,per_id = per_id)[[1]]
ESPNFromJSON <- get_data(leagueID = leagueID,per_id = per_id)[[2]]
schedule1 <- get_data(leagueID = leagueID,per_id = per_id)[[3]]
number_of_teams <- length(ESPNFromJSON$teams$id)
team_ids <- ESPNFromJSON$teams$id

proj.vs.act <- team_list %>% 
  filter(!(lineupSlot_id %in% c(20,21))) %>% 
  group_by(team, points_type) %>% 
  summarise(pts = sum(appliedTotal)) %>% 
  pivot_wider(names_from = "points_type", values_from = "pts") %>% 
  mutate(diff = round(actual - projected, digits = 2))


# Lineup slots
n_qb <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`0`
n_rb <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`2`
n_wr <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`4`
n_Sflex <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`7`
n_te <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`6`
n_dst <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`16`
n_k <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`17`

best_roster <- function(team_num = 1){
  base <-
    team_list %>% 
    dplyr::filter(points_type == "actual") %>%
    dplyr::filter(teamId == team_num) %>%
    unnest(eligibleSlots) %>% 
    dplyr::group_by(scoringPeriodId)
  
  rbs <-
    base %>% 
    dplyr::filter(eligibleSlots == 2) %>%
    slice_max(appliedTotal, n = n_rb)
  
  wrs <-
    base %>% 
    dplyr::filter(eligibleSlots == 4) %>%
    slice_max(appliedTotal, n = n_wr)
  
  tes <-
    base %>% 
    dplyr::filter(eligibleSlots == 6) %>%
    slice_max(appliedTotal, n = n_te)
  
  qbs <-
    base %>% 
    dplyr::filter(eligibleSlots == 0) %>%
    slice_max(appliedTotal, n = n_qb)
  
  def <-
    base %>% 
    dplyr::filter(eligibleSlots == 16) %>%
    slice_max(appliedTotal, n = n_dst)
  
  kik <-
    base %>% 
    dplyr::filter(eligibleSlots == 17) %>%
    slice_max(appliedTotal, n = n_k)
  
  flex <-
    base %>% 
    dplyr::filter(!fullName %in% c(rbs$fullName,wrs$fullName,tes$fullName,qbs$fullName,def$fullName,kik$fullName)) %>% 
    dplyr::filter(eligibleSlots == 7) %>% 
    slice_max(appliedTotal,n= n_Sflex)
  
  best_roster <-
    bind_rows(rbs,wrs,tes,qbs,def,kik,flex)  
  
  return(best_roster)
}

best_points <-
  team_ids %>% 
  map_dfr(~best_roster(team_num = .x)) %>% 
  dplyr::group_by(scoringPeriodId, team) %>% 
  dplyr::summarise(best_points = sum(appliedTotal))

proj.vs.act <- proj.vs.act %>% 
  left_join(best_points, by = c("team"))

proj.vs.act <- proj.vs.act %>% 
  mutate(Best_diff = percent(actual / best_points, digits = 2))

team_list <-
  1:per_id %>%
  # 2 %>%
  purrr::map_dfr(~get_data(leagueID = leagueID,per_id = .x)[[1]]) %>% 
  distinct(teamId,fullName,appliedTotal,scoringPeriodId,.keep_all = T)

team_list
# team_list_2 <- team_list

team_list %>% count(scoringPeriodId)
# team_list_2 %>% count(scoringPeriodId)

team_ids <- unique(team_list$teamId)

# team_list <-
# get_data(leagueID = leagueID,per_id = 1) %>% 
#   bind_rows(get_data(leagueID = leagueID,per_id = 2)) %>% 
#   distinct(teamId,fullName,appliedTotal,scoringPeriodId,.keep_all = T)

# team_list %>% dplyr::filter(team =="'R'm Chair Quarterback") %>% 
#   # as.data.frame() %>%
#   dplyr::filter(points_type=="actual") %>% dplyr::arrange(fullName)

# player_slot <- rep(1:roster_size,number_of_teams)
# team_number <- rep(1:number_of_teams,roster_size) %>% sort()

# team_list <- purrr::map2_dfr(player_slot,team_number, .f = ~player_extract(team_number = .y,player_number = .x)) %>%
#   left_join(schedule) %>% 
#   dplyr::mutate(points_type = if_else(str_length(externalId) > 6, "actual", "projected")) %>% 
#   relocate(team:appliedTotal, points_type)

# team_list %>% 
#   dplyr::filter(lineupSlot_id != 20) %>% dplyr::filter(lineupSlot_id==21)


schedule_prep <-
  team_list %>% 
  # as.data.frame() %>% 
  # dplyr::filter(team %in% c("'R'm Chair Quarterback")) %>%
  # dplyr::filter(team %in% c("Palindrome Tikkit")) %>%
  dplyr::filter(points_type=="actual")  %>%
  dplyr::filter(!lineupSlot_id %in% c(20,21)) %>%  # remove bench players
  dplyr::filter(scoringPeriodId <= per_id) %>% 
  dplyr::group_by(team, scoringPeriodId, points_type)  %>% 
  # dplyr::filter(scoringPeriodId==1) %>% 
  dplyr::summarise(points = sum(appliedTotal), gameId = gameId[1]) %>% 
  dplyr::arrange(scoringPeriodId,gameId) %>%
  dplyr::filter(points_type == "actual") %>% 
  dplyr::group_by(scoringPeriodId,gameId) 

# schedule_prep %>% as.data.frame() %>% dplyr::arrange(team)
standings <-
  schedule_prep %>% 
  dplyr::group_by(gameId) %>% 
  dplyr::mutate(Win_week = points==max(points)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(scoringPeriodId) %>% dplyr::mutate(med.pts = median(points)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(gameId) %>%
  dplyr::mutate(lucky = if_else(points==max(points) & max(points) < med.pts,1,0),
                unlucky = if_else(points!=max(points) & max(points) < med.pts,1,0)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(team) %>% dplyr::summarise(Wins = sum(Win_week), "Lucky Wins" = sum(lucky), "Unlucky Losses" = sum(unlucky)) %>%
  dplyr::mutate(Losses = per_id - Wins) %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(win_perc = Wins / (Wins + Losses))

# schedule_prep %>% as.data.frame() %>% dplyr::arrange(scoringPeriodId,points
week_win_standings <-
  schedule_prep %>% 
  dplyr::group_by(scoringPeriodId) %>% 
  dplyr::mutate(week_wins = rank(points)-1) %>% 
  dplyr::mutate(week_losses = max(week_wins)-week_wins) %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise(week_wins = sum(week_wins), week_losses = sum(week_losses)) %>% 
  dplyr::mutate(week_win_perc = week_wins / (week_wins + week_losses))

three_game_ave <-
  schedule_prep %>% 
  dplyr::select(team,scoringPeriodId,points) %>% 
  dplyr::filter(scoringPeriodId>(per_id-3)) %>%
  dplyr::group_by(team) %>% 
  dplyr::summarise(last_three=round(mean(points),1))


total_standings <-
  standings %>% 
  left_join(week_win_standings) %>% 
  left_join(three_game_ave) %>% 
  relocate(contains("perc"), .after = last_col()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(luck = (-win_perc + week_win_perc)/sqrt(2)) %>% 
  dplyr::mutate(win_perc = round(win_perc,4))
# select(-LastThree)

margins <- schedule_prep %>% 
  filter(scoringPeriodId == per_id) %>% 
  group_by(gameId) %>% 
  mutate(max.pts = max(points), min.pts = min(points)) %>% 
  ungroup() %>% 
  mutate(Margin = if_else(points == max.pts, max.pts - min.pts, min.pts - max.pts))

proj.vs.act <- proj.vs.act %>% 
  left_join(margins[, c(1,8)], by = "team")

luck_help_df = tibble(win_perc = c(0.3,0.3,.7,.7), week_win_perc = c(0,1,1,0), labs = c("Bad","Unlucky","Good","Lucky"))

plot_luck_chart <- function(total_standings = total_standings){
  total_standings %>% 
    dplyr::arrange(-luck) %>% 
    ggplot(aes(x=win_perc,y=week_win_perc, color = luck)) +
    geom_point(size = 3) +
    geom_abline(aes(intercept = 0,slope = 1)) +
    xlim(0,1) + ylim(0,1) +
    scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(total_standings$luck))) +
    # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
    geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
    geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) +
    labs(title = "How Lucky Is Your Team?",subtitle = "Are You Winning As Much As You Should?",
         x = "Traditional Win Percentage", y = "Every Game Every Week Win Percentage", color = "Luck") +
    theme(legend.position = "none")
  
}

luck_chart <- plot_luck_chart(total_standings = total_standings)
