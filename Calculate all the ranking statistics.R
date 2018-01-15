# Calculate all the ranking statistics
s = unique(subgraph[ , c("Name", "ID", "SemanticTypeIDs")])

c = unique(candidates[, c("Name", "ID", "SemanticTypeIDs")])
intermediate_table = unique(candidates[, c("Connected_to_ID", "Name", "ID", "SemanticTypeIDs")])

nConcepts = aggregate(intermediate_table$Connected_to_ID, by = list(ID = intermediate_table$ID), FUN = length)
colnames(nConcepts) = c("ID", "nConcepts")

## Calculate the ranking statistics
# Subgraph
nSources_total = sum(subgraph$nSources)
nRelationships_total = nrow(subgraph)

s$Edge_Chance_AB = 1/nrow(s)
s$Rel_Chance_AB = 0
s$Source_Chance_AB = 0

for(i in 1:length(s$ID)){
  subgroup = subgraph[which(subgraph$ID == s$ID[i]), ]
  s$Rel_Chance_AB[i] = nrow(subgroup) / nRelationships_total 
  s$Source_Chance_AB[i] = sum(subgroup$nSources) / nSources_total
}

ConIDs = unique(candidates$Connected_to_ID)
tempTab = data.frame()
for(i in 1:length(ConIDs)){
  print(i)
  subgroup = candidates[which(candidates$Connected_to_ID == ConIDs[i]),]  
  IdFreq = as.data.frame(table(ID = subgroup$ID))
  IdFreq$RelChance_BC = IdFreq$Freq / sum(IdFreq$Freq)
  IdFreq$EdgeChance_BC = 1/nrow(subgroup)
  IdSourceSums = aggregate(subgroup$nSources, by=list(ID = subgroup$ID), FUN=sum)
  IdSourceSums$SourceChance_BC = IdSourceSums$x / sum(subgroup$nSources)  
  IdFreq = merge(IdFreq[,-2], IdSourceSums[,-2], by = "ID")
  IdFreq$Connected_to_ID = ConIDs[i]
  tempTab = rbind(tempTab, IdFreq)
}

# Merge the subgraph statistics with the intermediate table
combined_table = merge(tempTab, s[,c("ID","Edge_Chance_AB","Rel_Chance_AB","Source_Chance_AB")], by.x = "Connected_to_ID", by.y = "ID", all.x = T)

## Calculate the cumulative chances
combined_table$RelChance_total = combined_table$Rel_Chance_AB * combined_table$RelChance_BC
combined_table$EdgeChance_total = combined_table$Edge_Chance_AB * combined_table$EdgeChance_BC
combined_table$SourceChance_total = combined_table$Source_Chance_AB * combined_table$SourceChance_BC

# Sum up the path-chances
agg_RelChance = aggregate(combined_table$RelChance_total, by = list(ID = combined_table$ID), FUN = sum)
colnames(agg_RelChance) = c("ID", "agg_rel")
agg_EdgeChance = aggregate(combined_table$EdgeChance_total, by = list(ID = combined_table$ID), FUN = sum)
colnames(agg_EdgeChance) = c("ID", "agg_edge")
agg_SourceChance = aggregate(combined_table$SourceChance_total, by = list(ID = combined_table$ID), FUN = sum)
colnames(agg_SourceChance) = c("ID", "agg_source")

# Create a combined outcome table
c$ID = as.factor(c$ID)
outcome_table = merge(c, agg_RelChance, by = "ID")
outcome_table = merge(outcome_table, agg_EdgeChance, by = "ID")
outcome_table = merge(outcome_table, agg_SourceChance, by = "ID")
outcome_table = merge(outcome_table, nConcepts, by = "ID")
outcome_table$score = 0

# Assign scores to the compounds
for(i in 1:nrow(all)) {
  if(all$ID[i] %in% c$ID){
    outcome_table[which(outcome_table$ID == all$ID[i]), "score"] = all$total[i]
  }}