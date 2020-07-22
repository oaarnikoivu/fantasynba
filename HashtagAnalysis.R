# Libraries
library(xlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

set.seed(43)

# Data

hashtag_data <- read.xlsx("/Users/olive/github/fantasynba/rankings.xlsx", 1)

# Data pre-processing

## Drop unnecessary columns 
drops <- c('R.', 'TEAM', 'GP', 'TOTAL')

hashtag_data <- hashtag_data %>%
  select(-one_of(drops))

## Remove text inside parantheses
hashtag_data$FG. <- gsub( " *\\(.*?\\) *", "", hashtag_data$FG.)
hashtag_data$FT. <- gsub( " *\\(.*?\\) *", "", hashtag_data$FT.)

head(hashtag_data)

## Change chr types to numeric
hashtag_data[,3:12] <- sapply(hashtag_data[,3:12], as.numeric)

# KMeans Clustering
data_cluster <- hashtag_data %>%
  select_if(is.numeric)

scaled_data <- scale(data_cluster)

row.names(scaled_data) <- hashtag_data$PLAYER

res.dist <- get_dist(scaled_data, stand = TRUE, method = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size= 2) + 
  theme_fivethirtyeight()

k <- 20

km.res <- kmeans(scaled_data, k, nstart = 20)

fviz_cluster(km.res, scaled_data, ellipse = TRUE, ellipse.alpha = 0.1, 
             palette = "jco", repel = TRUE, ggtheme = theme_fivethirtyeight(),
             main = FALSE, xlab = FALSE, ylab = FALSE,
             labelsize = 9, lwd = 2) + 
  theme_fivethirtyeight()

data.frame(sort(km.res$cluster)) %>% DT::datatable()

# Append the cluster to the existing dataset
cluster_data <- hashtag_data %>%
  mutate(Cluster = km.res$cluster)

## Helper functions
get_players_in_cluster <- function(cluster) {
  cluster_filter <- cluster_data %>%
    filter(Cluster == cluster)
  return(cluster_filter)
}

show_similarity_for_players_in_cluster <- function(cluster) {
  players <- cluster %>%
    select_if(is.numeric)
  row.names(players) <- cluster$PLAYER
  players.dist <- get_dist(players, stand = TRUE, method = "euclidean")
  fviz_dist(players.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size= 2) + 
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

visualize_stats_for_similar_players <- function(cluster) {
  stats <- cluster %>%
    gather(-c(PLAYER, POS, Cluster), key="var", value="value")
  
  ggplot(stats, aes(x=PLAYER, y=value)) + 
    geom_bar(stat="identity") +
    facet_wrap(~var, scales = "free") + 
    theme_fivethirtyeight() + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

# Categorize players into clusters

## BIGS
decentBigs <- get_players_in_cluster(1)
show_similarity_for_players_in_cluster(decentBigs)
visualize_stats_for_similar_players(decentBigs)

## SHOOTING BIGS
shootingBigs <- get_players_in_cluster(3)
show_similarity_for_players_in_cluster(shootingBigs)
visualize_stats_for_similar_players(shootingBigs)

## ALL AROUND BIGS
allAroundBigs <- get_players_in_cluster(4)
show_similarity_for_players_in_cluster(allAroundBigs)
visualize_stats_for_similar_players(allAroundBigs)

## BIGS DONT SHOOT -> punt assist, punt ft, punt 3pm, punt stl, low mpg 
bigsDontShoot <- get_players_in_cluster(7)
show_similarity_for_players_in_cluster(bigsDontShoot)
visualize_stats_for_similar_players(bigsDontShoot)

## OK BIGS - punt assist, low mpg - players to watch 
okBigs <- get_players_in_cluster(10)
show_similarity_for_players_in_cluster(okBigs)
visualize_stats_for_similar_players(okBigs)

## DOMINATING BIGS
dominatingBigs <- get_players_in_cluster(12)
show_similarity_for_players_in_cluster(dominatingBigs)
visualize_stats_for_similar_players(dominatingBigs)

## DEFENSIVE BEAST BIGS -> punt 3pm, meh to, very high rebounds, low ft, low assist 
defensiveBigs <- get_players_in_cluster(20)
show_similarity_for_players_in_cluster(defensiveBigs)
visualize_stats_for_similar_players(defensiveBigs)

## SHOOTING FORWARDS
shootingForwards <- get_players_in_cluster(2)
show_similarity_for_players_in_cluster(shootingForwards)
visualize_stats_for_similar_players(shootingForwards)

## DOMINATING FORWARDS - Requires further analysis
dominatingForwards <- get_players_in_cluster(5)
show_similarity_for_players_in_cluster(dominatingForwards)
visualize_stats_for_similar_players(dominatingForwards)

## MEH FORWARDS
mehForwards <- get_players_in_cluster(9)
show_similarity_for_players_in_cluster(mehForwards)
visualize_stats_for_similar_players(mehForwards)

## ALL AROUND GUARDS
allAroundGuards <- get_players_in_cluster(6)
show_similarity_for_players_in_cluster(allAroundGuards)
visualize_stats_for_similar_players(allAroundGuards)

## GOOD GUARDS
goodGuards <- get_players_in_cluster(19)
show_similarity_for_players_in_cluster(goodGuards)
visualize_stats_for_similar_players(goodGuards)

## HIGH ASSIST GUARDS <- punt FG, punt FT, punt TO, ignore 3pm 
highAssistGuards <- get_players_in_cluster(8)
show_similarity_for_players_in_cluster(highAssistGuards)
visualize_stats_for_similar_players(highAssistGuards)

## DOMINATING GUARDS - terrible turnovers, low fg, everything else is <3
dominatingGuards <- get_players_in_cluster(15)
show_similarity_for_players_in_cluster(dominatingGuards)
visualize_stats_for_similar_players(dominatingGuards)

## STUDS -> PUNT TURNOVERS, PUNT BLOCKS, GREAT ASSISTS, HIGH POINTS, ok FG
studs <- get_players_in_cluster(14)
show_similarity_for_players_in_cluster(studs)
visualize_stats_for_similar_players(studs)

## 3 POINT SAVAGES - low assist, very very low blocks, p good fg, good ft,
## great mpg, decent points, okay steals, very HIGH 3PM!
threePointSavuage <- get_players_in_cluster(18)
show_similarity_for_players_in_cluster(threePointSavuage)
visualize_stats_for_similar_players(threePointSavuage)

ggplot(threePointSavuage, aes(x=FG., y=PTS)) + geom_text(label=threePointSavuage$PLAYER, size=3, angle=45) +
  geom_smooth(method="auto", fill="blue") +
  theme_fivethirtyeight() 

ggplot(threePointSavuage, aes(x=MPG, y=FG., group=1)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 4) +
  geom_text(label=threePointSavuage$PLAYER, alpha=0.25, size=3, angle=45) +
  coord_flip() + 
  theme_fivethirtyeight()

sd(threePointSavuage$X3PM)

get_players_in_cluster(20) %>% DT::datatable()






