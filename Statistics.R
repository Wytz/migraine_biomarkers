library("Hmisc")

m = cbind(outcome_table_Rel$ID, outcome_table_Source$ID)

cor(m, method="kendall", use="pairwise") 

# Nog even kijken of ik deze toetsen moet gebruiken
rcorr(as.matrix(outcome_table[,c("agg_rel", "agg_edge", "agg_source", "nConcepts")]), type = "pearson")
rcorr(as.matrix(outcome_table[,c("agg_rel", "agg_edge", "agg_source", "nConcepts")]), type = "spearman")