## demo graph script
library(survivoR)
library(dplyr)
library(igraph)

boot_mapping <- boot_mapping
vote_history <- vote_history
season <- "Survivor: Borneo"
tribe <- "Tagi"
tribe_status <- "Original"
boot_mapping_original <- dplyr::filter(boot_mapping)
vote_history_boot_map_og <- inner_join(vote_history, boot_mapping_original, by = c('season' = 'season', 'episode' = 'episode', 'castaway' = 'castaway'))
vote_history_boot_map_og_tribe <- dplyr::filter(vote_history_boot_map_og, season_name.x == !!season, tribe.x == !!tribe, tribe_status.x == !!tribe_status)
season_before_graph <- vote_history_boot_map_og_tribe %>% select(castaway, vote)
season_before_graph <- season_before_graph[complete.cases(season_before_graph), ]
voters_s = season_before_graph[['castaway']]
voted_s = season_before_graph[['vote']]
season_ready_for_graph = c(rbind(voters_s, voted_s))
plot(graph(season_ready_for_graph))



