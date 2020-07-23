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

fviz_nbclust(scaled_data, kmeans, method = "wss")

apply_kmeans <- function(data, k, nstart) {
  set.seed(43)
  km.res <- kmeans(data, k, nstart)
}

show_clusters <- function(km.res, data) {
  fviz_cluster(km.res, data, ellipse = TRUE, ellipse.alpha = 0.1,
               palette = "jco", repel = TRUE,
               main = FALSE, xlab = FALSE, ylab = FALSE,
               labelsize = 9, lwd = 2) +
    theme_fivethirtyeight()
}

km.res <- apply_kmeans(data = scaled_data, k = 5, nstart = 20)
show_clusters(km.res, scaled_data)

data.frame(sort(km.res$cluster)) %>% DT::datatable()

# Append the cluster to the existing dataset
cluster_data <- hashtag_data %>%
  mutate(Cluster = km.res$cluster)

## Helper functions
get_players_in_cluster <- function(cluster, data) {
  cluster_filter <- data %>%
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

cluster_subset <- function(cluster) {
  subset <- get_players_in_cluster(cluster, data = cluster_data)
  select_subset <- subset %>%
    select_if(is.numeric) %>%
    select(-Cluster)
  scaled_subset <- scale(select_subset)
  row.names(scaled_subset) <- subset$PLAYER
  return(list(subset, scaled_subset))
}

create_new_cluster_subset <- function(cluster, k, nstart) {
  subset <- cluster_subset(cluster)[[1]]
  scaled_subset <- cluster_subset(cluster)[[2]]
  subset_km.res <- apply_kmeans(data = scaled_subset, k = k, nstart = nstart)
  print(show_clusters(subset_km.res, scaled_subset))
  return(list(subset, subset_km.res))
}

create_category <- function(data, k) {
  df <- data.frame()
  for (i in 1:k) {
    d <- get_players_in_cluster(i, data = data)
    df <- rbind(df, d)
  }
  return(df)
}

t <- create_new_cluster_subset(cluster = 5, k = 5, nstart = 20)[[2]]


## Categorize players into distinct clusters

studs <- create_new_cluster_subset(cluster = 3, k = 5, nstart = 20)
studsSubset <- studs[[1]] %>%
  mutate(Cluster = studs[[2]]$cluster) %>%
  arrange(-desc(Cluster))

centersCluster <- create_new_cluster_subset(cluster = 2, k = 5, nstart = 20)
centersClusterSubset <- centersCluster[[1]] %>%
  mutate(Cluster = centersCluster[[2]]$cluster) %>%
  arrange(-desc(Cluster))

shooters <- create_new_cluster_subset(cluster = 4, k = 5, nstart = 20)
shootersSubset <- shooters[[1]] %>%
  mutate(Cluster = shooters[[2]]$cluster) %>%
  arrange(-desc(Cluster))

allAroundForwardsAndCenters <- create_new_cluster_subset(cluster = 5, k = 5, nstart = 20)
allAroundForwardsAndCentersSubset <- allAroundForwardsAndCenters[[1]] %>%
  mutate(Cluster = allAroundForwardsAndCenters[[2]]$cluster) %>%
  arrange(-desc(Cluster))

# Punting Strategies

## Identify strong categories

visualize_variance <- function(players) {
  players_numeric <- players %>%
    select_if(is.numeric) %>%
    select(-Cluster)
  players_numeric <- scale(players_numeric)
  row.names(players_numeric) <- players$PLAYER
  ggplot(melt(players_numeric), aes(x=Var2, y=value)) + geom_boxplot(aes(fill=Var2), alpha=0.75) +
    geom_text(label=melt(players_numeric)$Var1, size=3, alpha=0.25, angle=45) + 
    theme_fivethirtyeight() + 
    labs(title = "Boxplot for each stat", 
         subtitle = "This graph visualizes the distribution, central value, and variability for each of the players stats.")
}

### Studs

##### Strong assists, TREB, FT. 
sx <- get_players_in_cluster(1, studsSubset)
visualize_stats_for_similar_players(sx)
summary(sx)
visualize_variance(sx)

##### Strong assists, FT, TREB, 3PM 
sy <- get_players_in_cluster(2, studsSubset)
visualize_stats_for_similar_players(sy)
summary(sy)
visualize_variance(sy)

##### Strong assists, PTS, 3PM, TREB
sz <- get_players_in_cluster(3, studsSubset)
visualize_stats_for_similar_players(sz)
summary(sz)
visualize_variance(sz)

#### Strong assists, STL, Low TO -> TODO
sk <- get_players_in_cluster(4, studsSubset)
visualize_stats_for_similar_players(sk)
summary(z)
visualize_variance(k)

#### Strong assists, FT, 3PM, TREB,
sj <- get_players_in_cluster(5, studsSubset)
visualize_stats_for_similar_players(sj)
summary(sj)
visualize_variance(sj)

### Centers

#### Terrible 3PM, Low MPG, Good BLK, Good TREB, Good FG
cx <- get_players_in_cluster(1, centersClusterSubset)
visualize_stats_for_similar_players(cx)
summary(cx)
visualize_variance(cx)

#### Terrible 3PM, Okay BLKS, Good FG, Terrible FT, Bad PTS, Great TREB
cy <- get_players_in_cluster(2, centersClusterSubset)
visualize_stats_for_similar_players(cy)
summary(cy)
visualize_variance(cy)

#### Great MPG, Great FG, Bad FT, No X3PM, Okay PTS, Amazing TREB, Amazing BLKS, Okay TO
##### DEFENSIVE 
cz <- get_players_in_cluster(3, centersClusterSubset)
visualize_stats_for_similar_players(cz)
summary(cz)

#### Okay MPG, Okay FG, Okay FT, Terrible 3PM, Meh PTS, Okay BLKS
#### ? Bigs
ck <- get_players_in_cluster(4, centersClusterSubset)
visualize_stats_for_similar_players(ck)
summary(ck)
visualize_variance(ck)

#### Okay MPG, Okay FG, Okay FT, Bad 3PM, Bad PTS
#### Rising BIGS
cj <- get_players_in_cluster(5, centersClusterSubset)
visualize_stats_for_similar_players(cj)
summary(cj)
visualize_variance(cj)

### Shooters

#### Okay AST, Okay BLK, Low FG, Great FT, Pretty good STL, Good TREB and very good 3PM 
shx <- get_players_in_cluster(1, shootersSubset)
visualize_stats_for_similar_players(shx)
visualize_variance(shx)

#### Okay AST, Terrible BLK, bad FG, great FT, low PTS, Great 3PM, okay TREB
shy <- get_players_in_cluster(2, shootersSubset)
visualize_stats_for_similar_players(shy)
visualize_variance(shy)

#### V good aST, low BLK, low FG, great FT, good PTS, quite a high TO, pretty good TREB, very good 3PM
shz <- get_players_in_cluster(3, shootersSubset)
visualize_stats_for_similar_players(shz)
visualize_variance(shz)

#### good ast, low blk, TERRIBLE fg, okay FT, Okay pts, good STL, good TREB, great X3PM
shk <- get_players_in_cluster(4, shootersSubset)
visualize_stats_for_similar_players(shk)
visualize_variance(shk)

#### Great AST, low BLK, poor fg, p good ft, low pts, high to, very good TREB, okay X3PM
shj <- get_players_in_cluster(5, shootersSubset)
visualize_stats_for_similar_players(shj)
visualize_variance(shj)

### All around forwards n centers

#### meh AST, Good BLK, not the greatest FG, meh FT, good PTS, Good TREB, good STL, Okay 3PM
afwx <- get_players_in_cluster(1, allAroundForwardsAndCentersSubset)
visualize_stats_for_similar_players(afwx)
visualize_variance(afwx)
summary(afwx)

#### Pretty goof AST, Good BLK, Okay FG, Meh FT, Low X3PM, Great TREB, Good STL, High TO
afwy <- get_players_in_cluster(2, allAroundForwardsAndCentersSubset)
visualize_stats_for_similar_players(afwy)
visualize_variance(afwy)
summary(afwy)

#### Good AST, Good BLK, Okay FG, Great FT, Great PTS, Okay STL, High TO, Great TREB, Okay X3PM
afwz <- get_players_in_cluster(3, allAroundForwardsAndCentersSubset)
visualize_stats_for_similar_players(afwz)
visualize_variance(afwz)

#### Good AST, Okay BLK, Meh FG, Okay FT, Good X3PM, Okay PTS, High TO, Great TREB, Great X3PM
afwk <- get_players_in_cluster(4, allAroundForwardsAndCentersSubset)
visualize_stats_for_similar_players(afwk)
visualize_variance(afwk)

#### Giannis and Andre Drummond 
afwj <- get_players_in_cluster(5, allAroundForwardsAndCentersSubset)
visualize_stats_for_similar_players(afwj)
visualize_variance(afwj)

#### Categorize players into 2 Punt Strategies

################## PUNT FG% and TO ######################
PUNT_FG_TO <- do.call("rbind", list(sx, sy, sz, sj, afwk))

PUNT_FG <- do.call("rbind", list(shx, afwx))

PUNT_TO <- do.call("rbind", list(afwz))

################## PUNT FG%, BLK, and PTS ######################
PUNT_FG_BLK_PTS <- do.call("rbind", list(shy))

################## PUNT FG%, BLK, and TO ######################
PUNT_FG_BLK_TO <- do.call("rbind", list(shz, shk, shj))

################## PUNT X3PM, AST and PTS ######################
PUNT_X3PM_AST_PTS <- do.call("rbind", list(cx, cy, cz, ck, cj)) 

################# PUNT X3PM, TO, FT #####################
PUNT_X3PM_TO_FT <- do.call("rbind",  list(afwy, afwj))

#################################################################################
visualize_variance(PUNT_X3PM_TO_FT)
visualize_stats_for_similar_players(PUNT_X3PM_TO_FT)






















