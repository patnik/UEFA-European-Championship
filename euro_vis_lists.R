setwd('C:/Users/User/Desktop/Euro_football')


int_results <- read.csv('results.csv')
str(int_results)

library(dplyr)
euro_results <- subset(int_results, tournament == 'UEFA Euro')
unique(euro_results$tournament)


# create a new column with the name of the winner
euro_results<- euro_results %>% mutate(winner = case_when(
  home_score > away_score ~ home_team,
  home_score < away_score ~ away_team))

# keep only the year
euro_results$year <- substring(euro_results$date,1,4)
unique(euro_results$winner)


colSums(is.na(euro_results))
nrow(euro_results)


library(tidyr)
# remove the draws from the dataset (they are the rows with NA in the column winner)
euro_results_clean <- euro_results %>% drop_na(winner)
nrow(euro_results_clean)

# convert the winner column as character- it wil be useful later
euro_results_clean$winner <- as.character(euro_results_clean$winner)

# in this way I find the winner of Euro in each year
euro_winner <- euro_results_clean %>% 
  group_by(year) %>% 
  slice(which.max(as.Date(date)))



# count wins by country and year
yearly_counts <- euro_results_clean %>%
  count(year, winner)

library(countrycode)
yearly_counts$code <- countrycode(yearly_counts$winner, "country.name",  "iso3c")

# attacht eh country code to Euro winners too
euro_winner$code <- countrycode(euro_winner$winner, "country.name",  "iso3c")

# manually add England, Scotland, Wales and Northen Ireland
yearly_counts <- 
  yearly_counts %>% mutate(code = ifelse(winner == "England", "ENG",
                                              ifelse(winner == "Wales", "WLS",
                                                     ifelse(winner == "Scotland", "SCT", 
                                                            ifelse(winner == "Northern Ireland", "NIR", code)))))

# split the dataset by year and create a list
spl_year <- split(yearly_counts, yearly_counts$year)


# nice link for formatting maps
# https://www.r-bloggers.com/zooming-in-on-maps-with-sf-and-ggplot2/
library(rnaturalearth)
library(rnaturalearthdata)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

ggplot() + 
  geom_sf(data = worldmap, color = "gray15", lwd = 0.3, fill=NA) +
  theme(panel.background = element_rect(fill='black',colour='black')) +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), datum = NA)



# makes some changes to worldmap to be able to match England, Scotland, Wales and Northen Ireland
worldmap2 <- 
  worldmap %>% mutate(adm0_a3_is = ifelse(geounit == "England", "ENG",
                                          ifelse(geounit == "Wales", "WLS",
                                                 ifelse(geounit == "Scotland", "SCT", 
                                                        ifelse(geounit == "Northern Ireland", "NIR", adm0_a3_is)))))



# keep only the column that we are interested
worldmap2 <- worldmap2 %>% select(adm0_a3_is)

# create a function that merges two tables
func <- function(x,y){merge(x, y, by.x=names(x)[4], by.y=names(y)[1])}
# and then merge every object list to the spatial data
spl_year_spatial <- lapply(spl_year, func, worldmap2)

# merge the Euro winners to the spatial data too
winners_data <- merge(euro_winner, worldmap2, by.x="code", by.y="adm0_a3_is", all.x=TRUE)
# split it by year
winners_data_year <- split(winners_data, winners_data$year)


library(ggplot2)
dir <- 'C:/Users/User/Desktop/Euro_football'


# create the maps with the nuber of wins
year <- 1960

for (n in seq(1:length(spl_year))) {
  mypath <- file.path(dir ,paste("Euro_wins_", year, ".png", sep = ""))
  png(file=mypath, width = 778, height = 496)
  plot(
    ggplot(spl_year_spatial[[n]]) +
      geom_sf(aes(fill = n, geometry = geometry), lwd = 0.1) +
      scale_fill_gradient(low = "#F5F5DC", high = "#FF0000", breaks=c(1,2,3,4,5)) +
      coord_sf(xlim = c(-20, 45), ylim = c(30, 73), datum = NA) +
      theme(plot.title = element_text(color = "white", size = 20, vjust = -10),
            plot.caption = element_text(color = "white", size = 8, vjust = 10),
            panel.background = element_rect(fill='black',colour='black'),
            legend.position = c(.1,.1), legend.direction = "horizontal",
            legend.background=element_blank(),
            legend.text=element_text(color='white', size = 12)) +
      labs(fill = 'Wins', title = paste ("Number of wins in UEFA Euro in", year),
           caption = 'Created by Nikos Patias') +
      guides(color=guide_legend(override.aes=list(fill=NA)))
    
  )
  year <- year + 4
  dev.off()
}






# create the maps with the winner
year <- 1960

for (n in seq(1:length(winners_data_year))) {
  mypath <- file.path(dir ,paste("Euro_winner_", year, ".png", sep = ""))
  png(file=mypath, width = 778, height = 496)
  plot(
    ggplot(winners_data_year[[n]]) +
      geom_sf(lwd = 0.1, fill = 'white',aes(geometry = geometry))+
      theme(panel.background = element_rect(fill='black',colour='black')) +
      coord_sf(xlim = c(-20, 45), ylim = c(30, 73), datum = NA) +
      theme(legend.position="none", plot.title = element_text(color = "white", size = 20, vjust = -20),
            plot.subtitle = element_text(color = "white", size = 30, vjust = -20, hjust = 0.5),
            plot.caption = element_text(color = "white", size = 8, vjust = 10)) +
      labs(title = paste ("Winner of UEFA Euro in", year),
           subtitle = winners_data_year[[n]][11],
           caption = 'Created by Nikos Patias')
    
  )
  year <- year + 4
  dev.off()
}





library(magick)
library(purrr)

list.files(path = dir , pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=0.5) %>% # animates, can opt for number of loops
  image_write("euro_wins.gif") # write to current dir





