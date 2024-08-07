## demo graph script
library(survivoR)
library(dplyr)
library(igraph)

boot_mapping <- boot_mapping
vote_history <- vote_history
#df[!duplicated(df), ]
season <- "Survivor: Fiji"
tribe <- "Moto - Original"
tribe_status <- "Original"
boot_mapping_original <- dplyr::filter(boot_mapping, boot_mapping$game_status == "In the game")
boot_mapping_original$tribe_unique <- paste(boot_mapping_original$tribe, "-", boot_mapping_original$tribe_status)
vote_history_boot_map_og <- inner_join(vote_history, boot_mapping_original, by = c('season' = 'season', 'episode' = 'episode', 'castaway' = 'castaway', 'version' = 'version'))
vote_history_boot_map_og <- vote_history_boot_map_og %>% select(version:season_name.y,tribe_unique)
vote_history_boot_map_og <- vote_history_boot_map_og[!duplicated(vote_history_boot_map_og), ]
vote_history_boot_map_og_tribe <- dplyr::filter(vote_history_boot_map_og, season_name.x == !!season, tribe_unique == !!tribe)
season_before_graph <- vote_history_boot_map_og_tribe %>% select(castaway, vote)
season_before_graph <- season_before_graph[complete.cases(season_before_graph), ]
voters_s = season_before_graph[['castaway']]
voted_s = season_before_graph[['vote']]
season_ready_for_graph = c(rbind(voters_s, voted_s))
plot(graph(season_ready_for_graph))


