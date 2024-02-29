
library (ecp)

#load results from python analysis:
#df_all_d <- arrow::read_feather('file.feather') 
library(readr)
mean_day_month <- read_csv("Relationship_advise_mean_day_month.csv")
print(mean_day_month,n=211)
#View(mean_day_month)
#View(advise_mix)
#get individual data frame for every measure:
df_comp = mean_day_month  ["compound"]
df_score = mean_day_month ["score"]
df_comment_len = mean_day_month ["comment_len"]
df_tox = mean_day_month ["Toxicity"]
df_sev_tox = mean_day_month ["Severe Toxicity"]
df_Profanity = mean_day_month ["Profanity"]
df_id_attack = mean_day_month ["Identity Attack"]
df_Insult = mean_day_month ["Insult"]
df_threat = mean_day_month ["Threat"]


#calculate break points using ecp algorithm 
#(significance_level = 0.05; minimum distance between observations = 50 days):
r_comp = e.divisive(X=df_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_score = e.divisive(X=df_score,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_comment_len = e.divisive(X=df_comment_len,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_tox = e.divisive(X=df_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_sev_tox = e.divisive(X=df_sev_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_Profanity = e.divisive(X=df_Profanity,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_id_attack = e.divisive(X=df_id_attack,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_Insult = e.divisive(X=df_Insult,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_threat = e.divisive(X=df_threat,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)




# Überprüfung
if (any(is.na(mean_day_month))) {
  print("Das DataFrame enthält mindestens eine Zeile mit NA-Werten.")
} else {
  print("Das DataFrame enthält keine NA-Werte.")
}

#give date of break points:
r_comp[["p.values"]]
r_comp[["estimates"]]
mean_day_month[c(r_comp$estimates),][,1]

r_score[["p.values"]]
r_score[["estimates"]]
mean_day_month[c(r_score$estimates),][,1]

r_comment_len[["p.values"]]
r_comment_len[["estimates"]]
mean_day_month[c(r_comment_len$estimates),][,1]

r_tox[["p.values"]]
r_tox[["estimates"]]
mean_day_month[c(r_tox$estimates),][,1]

r_sev_tox[["p.values"]]
r_sev_tox[["estimates"]]
mean_day_month[c(r_sev_tox$estimates),][,1]

r_Profanity[["p.values"]]
r_Profanity[["estimates"]]
mean_day_month[c(r_Profanity$estimates),][,1]

r_id_attack[["p.values"]]
r_id_attack[["estimates"]]
mean_day_month[c(r_id_attack$estimates),][,1]

r_Insult[["p.values"]]
r_Insult[["estimates"]]
mean_day_month[c(r_Insult$estimates),][,1]

r_threat[["p.values"]]
r_threat[["estimates"]]
mean_day_month[c(r_threat$estimates),][,1]
r_threat[["estimates"]]




################################# Relationship


library (ecp)



library(readr)
mean_day_month <- read_csv("Relationship_mean_day_month.csv")
print(mean_day_month,n=215)
#View(mean_day_month)
#View(advise_mix)
#get individual data frame for every measure:
df_comp = mean_day_month  ["compound"]
df_score = mean_day_month ["score"]
df_comment_len = mean_day_month ["comment_len"]
df_tox = mean_day_month ["Toxicity"]
df_sev_tox = mean_day_month ["Severe Toxicity"]
df_Profanity = mean_day_month ["Profanity"]
df_id_attack = mean_day_month ["Identity Attack"]
df_Insult = mean_day_month ["Insult"]
df_threat = mean_day_month ["Threat"]


#calculate break points using ecp algorithm 
#(significance_level = 0.05; minimum distance between observations = 50 days):
r_comp = e.divisive(X=df_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_score = e.divisive(X=df_score,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_comment_len = e.divisive(X=df_comment_len,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_tox = e.divisive(X=df_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_sev_tox = e.divisive(X=df_sev_tox,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_Profanity = e.divisive(X=df_Profanity,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_id_attack = e.divisive(X=df_id_attack,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_Insult = e.divisive(X=df_Insult,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)
r_threat = e.divisive(X=df_threat,sig.lvl=0.05,R=299,k=NULL,min.size=60,alpha=1)



# Beispiel: Ihr DataFrame df
# Annahme: Sie möchten überprüfen, ob es mindestens eine Zeile mit NA-Werten gibt

# Überprüfung
if (any(is.na(mean_day_month))) {
  print("Das DataFrame enthält mindestens eine Zeile mit NA-Werten.")
} else {
  print("Das DataFrame enthält keine NA-Werte.")
}

#give date of break points:
r_comp[["p.values"]]
r_comp[["estimates"]]
mean_day_month[c(r_comp$estimates),][,1]

r_score[["p.values"]]
r_score[["estimates"]]
mean_day_month[c(r_score$estimates),][,1]

r_comment_len[["p.values"]]
r_comment_len[["estimates"]]
mean_day_month[c(r_comment_len$estimates),][,1]

r_tox[["p.values"]]
r_tox[["estimates"]]
mean_day_month[c(r_tox$estimates),][,1]

r_sev_tox[["p.values"]]
r_sev_tox[["estimates"]]
mean_day_month[c(r_sev_tox$estimates),][,1]

r_Profanity[["p.values"]]
r_Profanity[["estimates"]]
mean_day_month[c(r_Profanity$estimates),][,1]

r_id_attack[["p.values"]]
r_id_attack[["estimates"]]
mean_day_month[c(r_id_attack$estimates),][,1]

r_Insult[["p.values"]]
r_Insult[["estimates"]]
mean_day_month[c(r_Insult$estimates),][,1]

r_threat[["p.values"]]
r_threat[["estimates"]]
mean_day_month[c(r_threat$estimates),][,1]



















