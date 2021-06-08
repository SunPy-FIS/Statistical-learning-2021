library(dplyr)
library(elo)
library(base)

rm(list=ls())
setwd("C:/Users/Amministratore/Desktop/Università/Anno 2/Statistical Learning/new")
match = read.csv("seriea2021.csv", header = T, stringsAsFactors = T, encoding = "UTF-8")[,-c(8:105)]

# ------------------------------------------------------------
# Dataset description
# ------------------------------------------------------------
dim(match)
str(match)
summary(match)
colnames(match)

# ------------------------------------------------------------
# Squad selection
# ------------------------------------------------------------
squad = data.frame(squad = unique(match$HomeTeam))

# Rating elo before the start (18/09/2020)
# http://clubelo.com/

elo = c(1640,1546,1579,1548,1633, 
        1835,1764,1583, 1580, 1586, 
        1839, 1491, 1760,1491,1745,
        1582,1557,1593,1726,1823)

squad = cbind(squad, elo)

# ------------------------------------------------------------
# Identification of the match results and subset selection
# ------------------------------------------------------------
match = match %>% mutate(result = if_else(FTHG > FTAG, 1, if_else(FTHG == FTAG, 0.5, 0)))
matches = match %>% select(Date, HomeTeam, AwayTeam, result)

# ------------------------------------------------------------
# Elo calculation during the championship
# ------------------------------------------------------------
n = dim(matches)[1]
elo_matrix = matrix(0, nrow = n, ncol = 2)
for (i in 1:n) {

  match = matches[i,]

  # Calculate elo pre-match
  teamA_elo = subset(squad, squad == match$HomeTeam)$elo
  teamB_elo = subset(squad, squad == match$AwayTeam)$elo
  
  # Elo update
  new_elo = elo.calc(wins.A = match$result, elo.A = teamA_elo, elo.B = teamB_elo, k = 20)

  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo = new_elo[1, 1]
  teamB_new_elo = new_elo[1, 2]
  elo_matrix[i,] = c(teamA_new_elo, teamB_new_elo)
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  squad = squad %>% mutate(elo = if_else(squad == match$HomeTeam, teamA_new_elo,if_else(squad == match$AwayTeam, teamB_new_elo, elo)))
  }

# ------------------------------------------------------------
# Associate teams elo to each match 
# ------------------------------------------------------------
matches$HomeTeam_elo = elo_matrix[,1]
matches$AwayTeam_elo = elo_matrix[,2]

squad = data.frame(squad = unique(matches$HomeTeam))
n_squad = dim(squad)[1]
n_matches = dim(matches)[1]
elo_trend = matrix(0, nrow = n_squad, ncol = 38)
for(i in 1:n_squad){
  supp = c()
  for(j in 1:n_matches){
    if((matches$HomeTeam[j] == squad[i,1])){
      supp = c(supp, matches$HomeTeam_elo[j])
    }    
    if((matches$AwayTeam[j] == squad[i,1])){
      supp = c(supp, matches$AwayTeam_elo[j])
    }
  }
  elo_trend[i,] = supp
}

# ------------------------------------------------------------
# Trend of elo for each squad during the championship
# ------------------------------------------------------------
final_matrix = as.data.frame(cbind(squad, elo, elo_trend))

rm(elo_matrix)
rm(elo_trend)
rm(new_elo)
rm(squad)

matches$HomeTeam_elo = rep(0,380)
matches$AwayTeam_elo = rep(0,380)

elo_assigment = function(team){
  
  # Extract elo vector from the final_matrix for this particular team
  elo_squad = final_matrix[final_matrix$squad == team, 2:dim(final_matrix)[2]]
  elo_squad = as.vector(t(elo_squad))
  
  # Convert final_matrix whick contains elo
  final_matrix = as.matrix(final_matrix[,2:dim(final_matrix)[2]])
  
  # Extract the index match for this particular team 
  idx_home = as.numeric(rownames(matches[(matches$HomeTeam == team),]))
  idx_away = as.numeric(rownames(matches[(matches$AwayTeam == team),]))
  idx_squad_match = sort(c(idx_home, idx_away))
  
  # Fill the matches dataframe with elo for this particular Team
  count = 1
  for(i in idx_squad_match){
    if(matches$HomeTeam[i] == team){
      matches$HomeTeam_elo[i] = elo_squad[count] 
    }
    if(matches$AwayTeam[i] == team){
      matches$AwayTeam_elo[i] = elo_squad[count] 
    }
    count = count + 1
  }
  matches
}

teams = unique(matches$HomeTeam)
for(team in teams){
  matches = elo_assigment(team)
}

write_csv(final_matrix, file = "EloMatrix.csv")























