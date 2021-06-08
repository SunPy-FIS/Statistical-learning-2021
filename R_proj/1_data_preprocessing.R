library(readr)
rm(list=ls())
setwd("C:/Users/Amministratore/Desktop/Università/Anno 2/Statistical Learning")
df = read.csv("players_21.csv", 
              header = T,
              na.strings = c("NA", "NAN", "nan", "na"))

# ------------------------------------------------------------
# Dataset description
# ------------------------------------------------------------
dim(df)
str(df)
summary(df)
colnames(df)

# ------------------------------------------------------------
# Select only usefull variables
# ------------------------------------------------------------
delete_columns= c("player_url", 
                  "long_name", 
                  "dob",
                  "body_type", 
                  "real_face", 
                  "release_clause_eur",
                  "player_tags", 
                  "team_jersey_number",
                  "loaned_from", 
                  "joined", 
                  "contract_valid_until", 
                  "nation_position",
                  "nation_jersey_number", 
                  "player_traits",
                  "defending_marking")
df_new = df[, !names(df) %in% delete_columns] 
df_new = df_new[,-c(66:91)]

# ------------------------------------------------------------
# Find na values in the dataset and omit them
# ------------------------------------------------------------
df_gk = df_new[df$player_positions=="GK",]
df_pl = df_new[df_new$player_positions!="GK",]

sum.is.na = function(vector){
  sum(is.na(vector))
}

apply(df_gk, 2, sum.is.na)
df_gk = na.omit(df_gk[,-c(21:26)])
apply(df_gk, 2, sum.is.na)

apply(df_pl, 2, sum.is.na)
df_pl = na.omit(df_pl[,-c(27:32)])
apply(df_pl, 2, sum.is.na)

# ------------------------------------------------------------
# Remova last 0 components
# ------------------------------------------------------------
df_gk[df_gk$value_eur == 0, "short_name"] = NA
df_gk = na.omit(df_gk)

df_pl[df_pl$value_eur == 0, "short_name"] = NA
df_pl = na.omit(df_pl)

# ------------------------------------------------------------
# New dataset in output
# ------------------------------------------------------------
write_csv(df, file = "players_21_preproc_pl.csv")
write_csv(df, file = "players_21_preproc_gk.csv")

























