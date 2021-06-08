rm(list=ls())
setwd("C:/Users/Amministratore/Desktop/Università/Anno 2/Statistical Learning/new")
elo = read.csv("EloMatrix.csv", header = T, stringsAsFactors = T, encoding = "UTF-8")
match = read.csv("new_seriea2021.csv", header = T, stringsAsFactors = T, encoding = "UTF-8")

# -----------------------------------------------------
# Select only Serie A players
# -----------------------------------------------------
seriea = read.csv("players_21_preproc_pl.csv", header = T, encoding = "UTF-8")

# -----------------------------------------------------
# One-hot encoding on the match results
# -----------------------------------------------------
n_match = dim(match)[1]
home_win = rep(0, n_match)
draw = rep(0, n_match)
away_win = rep(0, n_match)

match = cbind(match, home_win, draw, away_win)

match[match$result == 1, "home_win"] = 1
match[match$result == 0.5, "draw"] = 1
match[match$result == 0, "away_win"] = 1

# Change name to Hellas Verona
seriea[seriea$club_name=="Hellas Verona","club_name"] = "Verona"

# -----------------------------------------------------
# Assign attack-mid-defense parameters to each team
# -----------------------------------------------------
att_pos = c("LW","LF","CF","ST","RW","RF","LS","RS","LAM") 
mid_pos = c("LM","CDM","CM","CAM","RM","RCM","LCM","LDM","RAM","RDM","RES") 
def_pos = c("LB","LWB","RB","RWB","GK","LCB","RCB","CB") 

squad = as.data.frame(unique(match$HomeTeam), stringsAsFactors = T)
squad_list = unique(match$HomeTeam)
n_squad = length(squad_list)
att_squad = rep(0, n_squad)
mid_squad = rep(0, n_squad)
def_squad = rep(0, n_squad)
for(i in 1:n_squad){
  att = mid = def = 0
  att = mean(seriea[(seriea$team_position %in% att_pos) & (seriea$club_name == squad_list[i]), 
                    "overall"])
  mid = mean(seriea[(seriea$team_position %in% mid_pos) & (seriea$club_name == squad_list[i]), 
                    "overall"])
  def = mean(seriea[(seriea$team_position %in% def_pos) & (seriea$club_name == squad_list[i]), 
                    "overall"])
  att_squad[i] = att
  mid_squad[i] = mid
  def_squad[i] = def
}

# -----------------------------------------------------
# Rescale att-mid-def parameters based on elo ratio
# with the first match
# -----------------------------------------------------
rescale_matrix = matrix(0, nrow = (dim(elo)[1]), ncol = (dim(elo)[2]-1))
for(i in 1:n_squad){
  rescale_matrix[i,] = as.vector(t(elo[i,2:dim(elo)[2]]/elo[i,2]))
}
rescale_matrix = as.data.frame(cbind(squad, rescale_matrix))

name_squad = unique(match$HomeTeam)
att_matrix = t(as.matrix(t(att_squad)))
mid_matrix = t(as.matrix(t(mid_squad)))
def_matrix = t(as.matrix(t(def_squad)))

overall_matrix = (att_matrix+mid_matrix+def_matrix)/3 * rescale_matrix[,2:40]

# -----------------------------------------------------
# Assign to each match the related attack-mid-def strenght
# -----------------------------------------------------
match$home_overall_strength = rep(0, 380)
match$away_overall_strength = rep(0, 380)

j = 1
for(team in squad_list){
  count = 1
  for(i in 1:380){
    if(match$HomeTeam[i] == team){
      match$home_overall_strength[i] = overall_matrix[j,count]
      count = count+1
    }
    if(match$AwayTeam[i] == team){
      match$away_overall_strength[i] = overall_matrix[j,count]
      count = count+1
    }
  }
  j = j+1
}

################################
# INIZIARE A MODIFICARE DA QUI #
################################

# -----------------------------------------------------
# Model estimation
# -----------------------------------------------------
# diff = match$HomeTeam_elo-match$AwayTeam_elo

mod_win = glm(formula = home_win ~ home_overall_strength + away_overall_strength,
          family = "binomial", 
          data = match)
summary(mod_win)

mod_draw = glm(formula = draw ~ home_overall_strength + away_overall_strength,
              family = "binomial",
              data = match)
summary(mod_draw)
  
mod_loss = glm(formula = away_win~ home_overall_strength + away_overall_strength,
              family = "binomial", 
              data = match)
summary(mod_loss)

# ----------------------------------------------------
# Results
# -----------------------------------------------------
pred_win = predict(mod_win,type="response")
pred_draw = predict(mod_draw, type="response")
pred_loss = predict(mod_loss, type="response")

prediction = cbind(pred_win, pred_draw, pred_loss)
predicted_results = rep(0,380)
for(i in 1:380){
  if((prediction[i,1] > prediction[i,2]) & (prediction[i,1] > prediction[i,3])){
    predicted_results[i] = 1
  }
  if((prediction[i,2] > prediction[i,1]) & (prediction[i,1] > prediction[i,3])){
    predicted_results[i] = 0.5
  }
  if((prediction[i,3] > prediction[i,1]) & (prediction[i,1] > prediction[i,2])){
    predicted_results[i] = 0
  }
}
sum(predicted_results == match$result)/380

#quote = read.csv("seriea2021.csv",
#                 header = T,
#                 stringsAsFactors = T,
#                 encoding = "UTF-8")$B365H

#pred_win = predict(mod,type="response")

#plot(pred,quote)

















