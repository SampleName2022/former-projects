library(jsonlite)
library(tidyverse)
library(tibble)
library(psych)
library(StatsBombR)
library(caret)
library(ggsoccer)
library(caret)  #get back to caret for more models, valid og split evt.
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("jogall/soccermatics")
library(soccermatics)
options(scipen = 999)   #remove scientific notation e.g e100 -e1023 

#143 unikke kampe.#2159 spillere. det er ligeosm om der mangler to hold fra sæson 22 eLyngby og ac horsens??
unique(superliga_shots_3seasons$team.name)
unique(superliga_shots_3seasons$season_nr) 

# dataload ----------------------------------------------------------------

#comp id 77 (superliga) 

#dataload of plauerstats ikke events
superliga_2021_data <- fromJSON("bachelor.Rprojdata/playerstats_superliga_2021.json") #season id 90
superliga_2122_data <- fromJSON("data/superligastats/playerstats_superliga_2021.json") #seaon id108
superliga_2223_data <- fromJSON("data/playerstats_superliga_2223.json")  #...  235
superliga_2324_data <- fromJSON("data/superligastats/playerstats_superliga_2324.json") # ..... 281

head(superliga_2021_data)

# event data (her bruger jeg statsbomb apiet gennem en betalt bruger)
competitions <- competitions(, )
competitions <- subset(competitions, country_name == "Japan")

#superligacomps <- competitions %>% filter(competition_id == 77)
matchids_2122 <- matchesvector(username, password, 108, 77) #matches vector from statsbombr
matchids_2223 <- matchesvector(username, password, 235, 77)
matchids_2324 <- matchesvector(username, password, 281, 77)

#23.
japmatchs <- matchesvector(, , 107, 108)

japevents <- allevents(, , japmatchs)
japevents <- allclean(japevents)


japevents <- japevents %>% filter(type.name %in% c("Pass", "Carry"), player.name == "Mao Hosoya")


hosoyaheatmap <- japevents %>% soccerHeatmap(x = "location.x", y = "location.y", xBins = 16, yBins = 12,
                                                        title = "Heatmap over M.Hosoyas bevægelser i j1 ligaen 2023", 
                                                        subtitle = "Hosoya er centre forward angriber. Baseret på aflveringer og carries" ,kde = TRUE,colLow = "yellow1",
                                                        colHigh = "navy" , arrow = 'r',lengthPitch = 105, widthPitch = 68) 





  
superligaevents_2122 <- allevents(username, password, matchids_2122)
superligaevents_2122 <- allclean(superligaevents_2122)

superligaevents_2223 <- allevents(username, password, matchids_2223)
superligaevents_2223 <- allclean(superligaevents_2122)

superligaevents_2324 <- allevents(username, password, matchids_2324)
superligaevents_2324 <- allclean(superligaevents_2122)

#removing actions not moving the ball
superligaevents_2122 <- subset(superligaevents_2122, type.name %in%c("Pass","Carry","Shot") )
superligaevents_2122 <- subset(superligaevents_2122, play_pattern.name == "Regular Play")
superligaevents_2223 <- subset(superligaevents_2223, type.name %in%c("Pass","Carry","Shot") )
superligaevents_2223 <- subset(superligaevents_2223, play_pattern.name == "Regular Play")
superligaevents_2324 <- subset(superligaevents_2324, type.name %in%c("Pass","Carry","Shot") )
superligaevents_2324 <- subset(superligaevents_2324, play_pattern.name == "Regular Play")
superligaevents_2122$season_nr <- 2122
superligaevents_2223$season_nr <- 2223
superligaevents_2324$season_nr <- 2324


superligaevents_3seasons <- rbind(superligaevents_2122,superligaevents_2223,superligaevents_2324)
#removing actions not in regular play
superligaevents_3seasons <- subset(superligaevents_3seasons, play_pattern.name == "Regular Play")

table(bifevents_3seasons$type.name
      )

bifevents_3seasons <- subset(superligaevents_3seasons, team.name == "Brøndby IF")

matches <- get.matches(username = mi, password, season_id, competition_id)


# xt model ----------------------------------------------------------------


#bifevents_data <- fromJSON("data/bifeventsjson.json")
bifevents_actionsmove <- subset(bifevents_data, type.name %in%c("Pass","Carry","Shot") )
#library(StatsBombR)
clean_events <- allclean(bifevents_actionsmove)  #fra statsbombr pakke.

clean_events <- subset(clean_events, play_pattern.name == "Regular Play")

# beskrivende statistik del. lav heatmap---------------------------------------------------

#skud bif
#bifskud <- subset(clean_events, type.name == "Shot")
#shooters <- data.frame(table(bifskud$player.name))
describe(shooters)

#player played minutes
player_minutes <- bifevents_3seasons %>%
  group_by(match_id, player.name) %>%
  summarize(
    first_event = min(minute, na.rm = TRUE), # Adjust for missing data
    last_event = max(minute, na.rm = TRUE),
    minutes_played = last_event - first_event
  ) %>%
  ungroup()

#head
head(player_minutes) #player minutes til hver  kamp.
#


superliga

RoonyBardghji <-  superligaevents_3seasons %>% filter(player.name == "Roony Bardghji")


superliga_shots_3seasons <- subset(superligaevents_3seasons, type.name == "Shot")

superliga_shotoutcomes_notbif <- subset(superliga_shots_3seasons, team.name != "Brøndby IF")
superliga_shotoutcomes_notbif$shot.outcome.name

superliga_shots_perteam <- superliga_shotoutcomes_notbif %>% 
  group_by(team.name) %>% summarise(
    skududfald = list(table(shot.outcome.name))
  ) %>% 
  ungroup()


team_shot_outcomes <- superliga_shots_3seasons %>%
  group_by(team.name, shot.outcome.name) %>%    # Group by team and shot outcome
  summarize(count = n(), .groups = "drop") %>%  # Count occurrences
  pivot_wider(                                  # Reshape into wide format
    names_from = shot.outcome.name, 
    values_from = count,
    values_fill = 0                             # Fill missing combinations with 0
  ) %>%
  mutate(total_shots = rowSums(across(-team.name)))

write.csv2(team_shot_outcomes, "teamshotoutcomes.csv")

numerical_stats <- superliga_shots_3seasons %>%
  group_by(season_nr, team.name) %>%
  summarize(
    xG_stats = list(describe(shot.statsbomb_xg)),
    DistToGoal_stats = list(describe(DistToGoal)),
    ImpactHeight_stats = list(describe(shot_impact_height)),
    AngleToGoal_stats = list(describe(AngleToGoal))
  ) %>%
  ungroup()

# Summarize categorical columns
categorical_summary <- superliga_shots_3seasons %>%
  group_by(season_nr, team.name) %>%
  summarize(
    shot_outcome = list(table(shot.outcome.name)),
    players = list(table(player.name)),
    positions = list(table(position.name)),
    shot_types = list(table(shot.type.name))
  ) %>%
  ungroup()

# Combine the two summaries into a single list or dataframe
combined_summary <- list(
  Numerical_Stats = numerical_stats,
  Categorical_Stats = categorical_summary
)

# If you'd like a single dataframe with some key stats:
final_summary <- superligaevents_3seasons %>%
  group_by(season_nr, team.name) %>%
  summarize(
    mean_xG = mean(shot.statsbomb_xg, na.rm = TRUE),
    sd_xG = sd(shot.statsbomb_xg, na.rm = TRUE),
    mean_DistToGoal = mean(DistToGoal, na.rm = TRUE),
    sd_DistToGoal = sd(DistToGoal, na.rm = TRUE),
    mean_ImpactHeight = mean(shot_impact_height, na.rm = TRUE),
    sd_ImpactHeight = sd(shot_impact_height, na.rm = TRUE),
    mean_AngleToGoal = mean(AngleToGoal, na.rm = TRUE),
    sd_AngleToGoal = sd(AngleToGoal, na.rm = TRUE),
    shot_outcomes = list(table(shot.outcome.name)),
    player_counts = list(table(player.name)),
    position_counts = list(table(position.name)),
    shot_type_counts = list(table(shot.type.name))
  ) %>%
  ungroup()


#fck top player. #hvorfor er lyngby der ikke??

###3


# Define pitch dimensions  --- using statsbomb dimensions.
pitch_length <- 120 #StatsBomb har 120x80 dimensions
pitch_width <- 80

# grid cells for transition map
grid_x <- seq(0, pitch_length, length.out = 17) #16 vertical zones  pitch grid 16x12, 13 17  breaks,
grid_y <- seq(0, pitch_width, length.out = 13) #12 horizontal zones

#nu bygger jeg min xt model. først en for indeværende sæson her med alle kampe, og så en baseret på brøndbys events. har måske brug for mere data?

#
bifevents_3seasons <- superligaevents_3seasons %>% filter(team.name == "Brøndby IF")


#assign events to grid cells
bifevents_3seasons <- bifevents_3seasons %>%
  mutate(
    grid_x = cut(location.x, breaks = grid_x, labels = 1:16, include.lowest = TRUE),
    grid_y = cut(location.y, breaks = grid_y, labels = 1:12, include.lowest = TRUE)
  )


 #possession = possesion id

# Ensure possession chains are correctly grouped


#i mit data er der 238 chains. 
#onto xt class model
# Prepare data for modeling
# Ensure possession chains are correctly grouped
bif_chain_events <- bifevents_3seasons %>%
  group_by(possession) %>%
  mutate(
    chain_start = first(row_number()), #første event
    chain_end = last(row_number())    #sodste event in chain
  )




#chain outcomes - se possession (chain) slutter i shot eller ej
bif_chain_outcomes <- bif_chain_events %>%
  summarize(
    ends_in_shot = any(type.name == "Shot"), #event is shot or na??
    chain_xT = 0 # Dummy xt for nu Predicter senere.
  )


superliga_chain_events <- superligaevents_3seasons %>%
  group_by(possession) %>%
  mutate(
    chain_start = first(row_number()), 
    chain_end = last(row_number())    #last obs. via row number
  )


superliga_chain_outcomes <- superliga_chain_events %>%
  summarize(
    ends_in_shot = any(type.name == "Shot"), #event is shot or na??
    chain_xT = 0 # Dummy xt for nu Predicter senere.
  )

#i mit data er der 238 chains. 
#onto xt class model

#
bif_chain_events <- bif_chain_events %>%
  left_join(bif_chain_outcomes, by = "possession") #ends in shot ja nej, t f

superliga_chain_events <- superliga_chain_events %>%
  left_join(superliga_chain_outcomes, by = "possession")


# bif class predict model -------------------------------------------------


#glm for nu. forsøg xgboost + rf etc. vælg nok en der er til at forklare nemt til eksamen hvis censor. rf lidt kringlet med coef
model_data <- bif_chain_events %>%
  filter(type.name %in% c("Pass", "Carry")) %>%
  group_by(possession) %>%
  summarize(
    start_grid_x = first(grid_x),
    start_grid_y = first(grid_y),
    ends_in_shot = max(ends_in_shot)
  )





#bif_clean_events <-  subset(clean_events, team.id == 559)

playerstats2324df <- fromJSON("data/superligastats/playerstats_superliga_2324.json")

# Prepare data for modeling


# using bif shot model ----------------------------------------------------


# Generate a grid of all possible cells
grid_cells <- grid_cells %>%
  mutate(
    start_grid_x = factor(start_grid_x, levels = levels(model_data$start_grid_x)),
    start_grid_y = factor(start_grid_y, levels = levels(model_data$start_grid_y))
  )


#predict respons
grid_cells$xT <- predict(bif_shot_model, newdata = grid_cells, type = "response")   #det her er baseline xt for de diverse grid cells



allactions_eventdf <- read.csv("data/superliga_brøndby_2324.csv")


# using general shot model ------------------------------------------------


ggplot(grid_cells, aes(x = start_grid_x, y = start_grid_y, fill = xT)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Expected Threat (xT) Model", x = "Pitch Grid X", y = "Pitch Grid Y")  #chat gpt ggplot. giver ikke helt mening. 
#løs med mit python heatmap eller ggsoccer istedet - oonvert de to ends in shot til 4 df, events dont end in shot, events end in shot, for superliga/bif (04/12)



# assigning possession xt to every event ----------------------------------

grid_x_breaks <- seq(from = min(bifevents_3seasons$location.x, na.rm = TRUE), 
                     to = max(clean_events$location.x, na.rm = TRUE), 
                     length.out = 17)  #16 felt celler,  17 breaks  (+1)

grid_y_breaks <- seq(from = min(bifevents_3seasons$location.y, na.rm = TRUE), 
                     to = max(bifevents_3seasons$location.y, na.rm = TRUE), 
                     length.out = 13)  # 12 cells,  13 breaks

bifevents_3seasons <- bifevents_3seasons %>%
  mutate(
    xT_start = predict(bif_shot_model, newdata = data.frame(
      start_grid_x = grid_x,
      start_grid_y = grid_y
    ), type = "response")
  )


bif_csv_df <-read.csv("/") 

#assigner xt via model her. brug end xt som er real xt gen
#dener dog meget komapkt!!!
bifevents_3seasons <- bifevents_3seasons %>%
  mutate(
    grid_x = factor(grid_x, levels = levels(model_data$start_grid_x)),
    grid_y = factor(grid_y, levels = levels(model_data$start_grid_y))
  ) %>%
  
  #end locations alt efter pass eller carry. i tvivl om nødvendigt
  mutate(
    end_grid_x = case_when(
      type.name == "Pass" & !is.na(pass.end_location.x) ~ cut(pass.end_location.x, breaks = grid_x_breaks, labels = 1:16, include.lowest = TRUE),
      type.name == "Carry" & !is.na(carry.end_location.x) ~ cut(carry.end_location.x, breaks = grid_x_breaks, labels = 1:16, include.lowest = TRUE),
      TRUE ~ NA_character_ # If not a Pass or Carry, no end location
    ),
    end_grid_y = case_when(
      type.name == "Pass" & !is.na(pass.end_location.y) ~ cut(pass.end_location.y, breaks = grid_y_breaks, labels = 1:12, include.lowest = TRUE),
      type.name == "Carry" & !is.na(carry.end_location.y) ~ cut(carry.end_location.y, breaks = grid_y_breaks, labels = 1:12, include.lowest = TRUE),
      TRUE ~ NA_character_ # If not a Pass or Carry, no end location
    )
  ) %>%
  
  #end_grid_x + end_grid_y til factors ligesom bif shot model
  mutate(
    end_grid_x = factor(end_grid_x, levels = levels(model_data$start_grid_x)),
    end_grid_y = factor(end_grid_y, levels = levels(model_data$start_grid_y))
  ) %>%
#predict
  mutate(
    xT_end = ifelse(!is.na(end_grid_x) & !is.na(end_grid_y), 
                    predict(bif_shot_model, newdata = data.frame(
                      start_grid_x = end_grid_x,
                      start_grid_y = end_grid_y
                    ), type = "response"), 
                    NA) # na hvis ingen end location
  )

#gør det samme for superliga events - men hvor vi bruger BIF shot model stadig, på deres events
superliga_grid_x_breaks <- seq(from = min(superligaevents_3seasons$location.x, na.rm = TRUE), 
                     to = max(superligaevents_3seasons$location.x, na.rm = TRUE), 
                     length.out = 17)  #16 felt celler,  17 breaks  (+1)

superliga_grid_y_breaks <- seq(from = min(superligaevents_3seasons$location.y, na.rm = TRUE), 
                     to = max(superligaevents_3seasons$location.y, na.rm = TRUE), 
                     length.out = 13)  # 12 cells,  13 breaks

superligaevents_3seasons <- superligaevents_3seasons %>%
  mutate(
    xT_start = predict(bif_shot_model, newdata = data.frame(
      start_grid_x = grid_x,
      start_grid_y = grid_y
    ), type = "response")
  )

# Assign xT via model here. Use end xT as the real xT generated.
superligaevents_3seasons <- superligaevents_3seasons %>%
  mutate(
    grid_x = factor(grid_x, levels = levels(model_data$start_grid_x)),
    grid_y = factor(grid_y, levels = levels(model_data$start_grid_y))
  ) %>%
  
  # End locations based on Pass or Carry
  mutate(
    end_grid_x = case_when(
      type.name == "Pass" & !is.na(pass.end_location.x) ~ cut(pass.end_location.x, breaks = grid_x_breaks, labels = 1:16, include.lowest = TRUE),
      type.name == "Carry" & !is.na(carry.end_location.x) ~ cut(carry.end_location.x, breaks = grid_x_breaks, labels = 1:16, include.lowest = TRUE),
      TRUE ~ NA_character_ # If not a Pass or Carry, no end location
    ),
    end_grid_y = case_when(
      type.name == "Pass" & !is.na(pass.end_location.y) ~ cut(pass.end_location.y, breaks = grid_y_breaks, labels = 1:12, include.lowest = TRUE),
      type.name == "Carry" & !is.na(carry.end_location.y) ~ cut(carry.end_location.y, breaks = grid_y_breaks, labels = 1:12, include.lowest = TRUE),
      TRUE ~ NA_character_ # If not a Pass or Carry, no end location (eller det er der, men det er et shot, og ikke til xt.)
    )
  ) %>%
  
  # Convert end_grid_x + end_grid_y to factors like bif_shot_model
  mutate(
    end_grid_x = factor(end_grid_x, levels = levels(model_data$start_grid_x)),
    end_grid_y = factor(end_grid_y, levels = levels(model_data$start_grid_y))
  ) %>%
  
  # Predict xT_end
  mutate(
    xT_end = ifelse(!is.na(end_grid_x) & !is.na(end_grid_y), 
                    predict(bif_shot_model, newdata = data.frame(
                      start_grid_x = end_grid_x,
                      start_grid_y = end_grid_y
                    ), type = "response"), 
                    NA) # NA if no end location
  )


bifevents_3seasons <- bifevents_3seasons %>%
  select(-shot.freeze_frame, 
         -shot.one_on_one, 
         -pass.pass_cluster_id, 
         -pass.pass_cluster_probability, 
         -player.id.GK)

superligaevents_3seasons <- superligaevents_3seasons %>%
  select(-shot.freeze_frame, 
         -shot.one_on_one, 
         -pass.pass_cluster_id, 
         -pass.pass_cluster_probability, 
         -player.id.GK)


#   
describe(bifgoals$shot.statsbomb_xg)

superliga_true_shotend <- superligaevents_3seasons %>%
  group_by(possession, match_id) %>%
  filter(last(type.name == "Shot")) %>% # Check if the last event is a shot
  ungroup()

# Group by possession and filter for possessions that do not end in a shot
superliga_false_shotend <- superligaevents_3seasons %>%
  group_by(possession, match_id) %>%
  filter(!last(type.name == "Shot")) %>% # Check if the last event is not a shot
  ungroup()

bifevents_true_shotend <- bifevents_3seasons %>%
  group_by(possession, match_id) %>%
  filter(last(type.name == "Shot")) %>% # Check if the last event is a shot
  ungroup()

# Group by possession and filter for possessions that do not end in a shot
bifevents_false_shotend <- bifevents_3seasons %>%
  group_by(possession, match_id) %>%
  filter(!last(type.name == "Shot")) %>% # Check if the last event is not a shot
  ungroup()


bif_goodshots <- bifevents_true_shotend %>% 
  group_by(possession,match_id) %>% 
  filter(last(shot.outcome.name %in% c("Goal", "Saved"))) %>% 
  ungroup()

bif_highxg <- bifevents_true_shotend %>% 
  group_by(possession,match_id) %>% 
  filter(last(shot.statsbomb_xg > 0.2)) %>% 
  ungroup()

#compare passes




superliga_goodshots <- superliga_true_shotend %>% 
  group_by(possession,match_id) %>% 
  filter(last(shot.outcome.name %in% c("Goal", "Saved"))) %>% 
  ungroup()

bif_goodshots_overmedianxg <- bif_goodshots %>% 
  group_by(possession,match_id) %>% 
  filter(last(shot.statsbomb_xg > 0.15)) %>% 
  ungroup()

superliga_goodshots_highxg <- superliga_goodshots %>%
  group_by(possession,match_id) %>% 
  filter(last(shot.statsbomb_xg > 0.15)) %>% 
  ungroup()

superliga_goodshots_overmedianxg_slicebif <- superliga_goodshots_highxg[1:850,]   #slicer til lignende strrrelse for bif, sidste et shot.
#
table(superliga_goodshots_overmedianxg_slicebif$type.name)  # carry:  375, pass: 401, shot: 67 total 843
table(bif_goodshots_overmedianxg$type.name) #carry:330, pass: 387, shot: 111. total 828


set.seed(1234)
#using caret to make a reasonable split for plotting

#superligaevents_3seasons$is_shot <- ifelse(superligaevents_3seasons$type.name== "Shot", TRUE, FALSE)

#superliga_goals$is_shot <- ifelse(superliga_goals$type.name== "Shot", TRUE, FALSE)


# heatmapsssss -----------------------------------------------------------------

bifmap <- bif_goodshots_overmedianxg %>% soccerHeatmap(x = "location.x", y = "location.y", xBins = 16, yBins = 12,
                title = "BIF poss.chains mod skud der resulterede i mål eller GK save", 
                subtitle = "Heatmap for BIF events på ~850 begivenheder,i de 3 sæsoner" ,kde = TRUE,colLow = "blue" , arrow = 'r')


superligamap <- superliga_goodshots_overmedianxg_slicebif %>% soccerHeatmap(x = "location.x", y = "location.y", xBins = 16, yBins = 12,
                             title = "Superliga poss.chains mod skud der resulterede i mål eller GK save", 
                             subtitle = "Heatmap for Superliga events, ~850 begivenheder i de 3 sæsoner",
                              kde = TRUE,colLow = "blue" #colours
                           ,arrow = "r",lengthPitch = 120, widthPitch = 80) #density 
bifmap / superligamap

soccerSpokes(superliga_goodshots_overmedianxg_slicebif, 
             xBins = 16, yBins = 12, angleBins = 8, legend = T, title = "Spoke template title", plot = superligamap, 
              x = "location.x", y = "location.y", angle = "pass.angle", theme = "dark")
#in my default directory
write_json(bifevents_true_shotend,  path = "data/bifevents_endsshot.json")
write_json(bifevents_false_shotend, path = "data/bifevents_no_endsshot.json")
write_json(superliga_true_shotend,  path = "data/superliga_endsshot.json")
write_json(superliga_false_shotend,  path = "data/superliga_no_endsshot.json")
write_json(bifevents_3seasons_subset,  path = "data/bif_cleanevents_3seasons_subset.json")

bifevents_3seasons_subset <- bifevents_3seasons[1:15000,]



#ift. problemformulering.
relevantpositons <- c("Right Center Midfield", "Left Center Midfield" , "Right Center Forward", "Center Forward", 
     "Center Attacking Midfield", "Right Wing", "Left Wing", "Left Attacking Midfield", 
     "Right Attacking Midfield", "Right Midfield", "Left Midfield", "Center Defensive Midfield", 
     "Left Center Midfield", "Right Center Midfield", "Center Defensive Midfield", "Left Midfield", "Right Midfield")

notrelevantpositions <- c("Goalkeeper", "Right Back", "Left Back", 
                          "Right Center Back", "Left Wing Back", "Right Wing Back", "Center Back" ,  "Left Center Back")


#xt per event. jeg tror jeg går med chain xt i stedet og dividerer ud per spiller. (team play - se ba)
event_xt_data <- clean_events %>%
  # Select relevant columns
  select(
    possession, 
    possession_team.id, 
    possession_team.name, 
    player.name, 
    type.name, 
    shot.statsbomb_xg, 
    xT_start, 
    xT_end
  ) %>%
  #xt change.
  mutate(
    xT_change = xT_start - xT_end
  )

# View the new dataframe
head(event_xt_data)

write_json(event_xt_data, "data/event_xt_data.json")


#hvor lang tid har hver spiller til senere
minutes_per_player <- player_minutes %>%
  group_by(player.name, match_id) %>%
  summarize(
    minutes_played = sum(minutes_played, na.rm = TRUE)  
  )

#xtper player df
xt_per_player <- clean_events %>%
  group_by(player.name, match_id) %>%
  summarize(
    total_xT = sum(xT_start - xT_end, na.rm = TRUE)  #
  ) %>%
  #combinationzz
  left_join(minutes_per_player, by = c("player.name", "match_id")) %>%
  # Calculate xT per 90 minutes played
  mutate(
    xT_per_90 = total_xT / (minutes_played / 90)
  )

head(xt_per_player)
# glimpse() 
#describe()
# Create player_season_xt by summing total_xT and minutes_played, and calculating xT_per_90
player_season_xt <- xt_per_player %>%
  group_by(player.name) %>%
  summarize(
    total_xT = sum(total_xT, na.rm = TRUE),  # Sum of xT across all matches for the player
    minutes_played = sum(minutes_played, na.rm = TRUE),  # Sum of minutes played across all matches
    xT_per_90 = sum(total_xT, na.rm = TRUE) / (sum(minutes_played, na.rm = TRUE) / 90)  # Average xT per 90 minutes
  )

# View the resulting dataframe
head(player_season_xt)




# descriptive statistics --------------------------------------------------


#descriptive statistics on whole season all players - variables with psych package.

superliga2021statistic <- describe(superliga_2223_data[16:224])
superliga2021statistic <- rownames_to_column(superliga2021statistic)

#072f67 #brøndby cool blue






head(brondbyshotssmall)


# plotting ----------------------------------------------------------------
superligashots_small <- superliga_shots_3seasons %>% 
  select(season_nr, AngleToGoal, DistToGoal, team.name, shot.statsbomb_xg, 
         location.x, location.y, location.x.GK, location.y.GK,
         shot.outcome.name, match_id, shot.technique.name, player.name, period)

brondbyshotssmall <- superligashots_small %>%  filter(team.name == "Brøndby IF")




filtered_data <- brondbyshotssmall %>%
  filter(shot.outcome.name %in% c("Saved", "Goal"))

filtered_data_not <- brondbyshotssmall %>%
  filter(!shot.outcome.name %in% c("Saved", "Goal"))
# Create the first histogram (for filtered data)
plot_filtered <- ggplot(filtered_data, aes(x = shot.statsbomb_xg)) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
    binwidth = 0.025, 
    fill = "#072f67", 
    color = "black", 
    alpha = 0.9
  ) + ylim(0,40) + xlim(NA,1)+
  labs(
    title = "Skud på mål (Mål eller reddet af GK)",
    x = "Expected Goals (xG), grupperinger af 0,025 stigning",
    y = "Frekvens i % af total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 10)
  )

#combinations
# Create the histogram plot with both groups
plot_combined <- ggplot() +
  # Add histogram for superligashots_small
  geom_histogram(
    data = superligashots_small,
    aes(x = shot.statsbomb_xg, y = after_stat(count) / sum(after_stat(count)) * 100),
    binwidth = 0.025,
    fill = "#e233ff",  # Purple color
    color = "black",
    alpha = 1  # Slightly transparent
  ) +
  # Add histogram for filtered_data
  geom_histogram(
    data = filtered_data,
    aes(x = shot.statsbomb_xg, y = after_stat(count) / sum(after_stat(count)) * 100),
    binwidth = 0.025,
    fill = "#072f67",  # Existing blue color
    color = "black",
    alpha = 0.7  # Slightly transparent
  ) +
  ylim(0, 40) + xlim(NA, 1) +
  labs(
    title = "Histogram med sammenligning af xG (Brøndby IF vs Superligaen)",
    x = "Expected Goals (xG), grupperinger af 0,025 stigning",
    y = "Frekvens i % af total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 10)
  )

# Create the second histogram (for shots not in filtered data)
plot_filtered_not <- ggplot(filtered_data_not, aes(x = shot.statsbomb_xg)) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
    binwidth = 0.025, 
    fill = "#ffff00", 
    color = "black", 
    alpha = 0.7
  ) + ylim(0,40) + xlim(NA,1) +
  labs(
    title = "Skud som er off target, eller blokeret af andre spillere",
    x = "Expected Goals (xG), grupper af 0,025 stigning",
    y = "Frekvens i % af total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 10)
  )

library(patchwork)
# Combine the two plots vertically using patchwork
plot_filtered / plot_filtered_not


write_json(superligashots_small, path = "data/superligashots_small.json")

# laver histogram på xg, laver så heatmap ud fra det.


#setting statname as variable.
superliga2122statistic <- describe(superliga_2223_data[16:224])
superliga2122statistic <- rownames_to_column(superliga2122statistic)
superliga2223statistic <- describe(superliga_2223_data[16:224])
superliga2223statistic <- rownames_to_column(superliga2223statistic)
superliga2324statistic <- describe(superliga_2223_data[16:224])
superliga2324statistic <- rownames_to_column(superliga2324statistic)

# violin plots on key stats of offensive players - to show differences??

#variables which might come in handy.s
player_season_np_shots_90
player_season_xa_90
player_season_np_xg_90
player_season_assists_90
player_season_op_passes_into_and_touches_inside_box_90
player_season_sp_key_passes_90
