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
