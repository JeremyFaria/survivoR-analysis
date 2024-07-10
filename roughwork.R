library(survivoR)
library(dplyr)
library(stringr)
library(ggplot2)
library(usmap)
library(plotly)
castaway_details <- castaway_details
us_castaways <- dplyr::filter(castaway_details, grepl("^US", castaway_details$castaway_id))
castaways_id_season <- dplyr::select(castaways, castaway_id, season)
us_castaways_full <- left_join(us_castaways, castaways_id_season, by = "castaway_id")
bipoc_per_season <- us_castaways_full  %>% group_by(season) %>% summarise(bipoc_percentage = (sum(bipoc)/length(unique(castaway_id)) * 100))
ggplot() + geom_bar(aes(x=bipoc_per_season$season,y=bipoc_per_season$bipoc_percentage), stat="identity")

castaway_by_state <- dplyr::filter(castaways, grepl("^US", castaways$castaway_id)) %>% count(state)
castaway_by_state_clean <- merge(castaway_by_state, usmap::statepop, by.x = 'state', by.y = 'full')
usmap::plot_usmap(data = castaway_by_state_clean, values = "n", color = "grey") +
  scale_fill_distiller("Castaways per state", trans="reverse", limits=c(218,0))

screen_time <- screen_time
ggplot(data = screen_time, aes(x = episode, y = screen_time, group = castaway_id)) +
  geom_line(linetype = "dashed")+
  geom_point(color="red")

screen_time_players <- inner_join(screen_time, castaway_details)

ggplot(data = screen_time_players, aes(x = episode, y = screen_time, group = castaway_id, color = bipoc)) +
  geom_line() +
  geom_point(color="red")


boot_mapping <- boot_mapping
screen_time_players_boot <- inner_join(screen_time_players, boot_mapping, by = c('episode' = 'episode', 'castaway_id'= 'castaway_id'))
screen_time_players_boot_ingame <- dplyr::filter(screen_time_players_boot, game_status == 'In the game') %>% select(-final_n, -order) %>% distinct()
ggplot(data = screen_time_players_boot_ingame, aes(x = episode, y = screen_time, group = castaway_id, color = bipoc)) +
  geom_line() +
  geom_point(color="red")

screen_time_players_boot_ingame_grouped <- screen_time_players_boot_ingame %>% group_by(bipoc, episode) %>% summarise(average_screen_time = mean(screen_time))
ggplot(data = screen_time_players_boot_ingame_grouped, aes(x = episode, y = average_screen_time, color = bipoc)) +
  geom_line() +
  geom_point(color = "red")


ggplot(data = screen_time_players_boot_ingame_grouped) +
  geom_bar(aes(fill = bipoc,x=episode,y=average_screen_time), stat="identity", position = "dodge")

vote_history <- vote_history
