require(ggplot2)

# Write away the data and visualize it in tables and figures

outcome_table_Rel = outcome_table[order(outcome_table$agg_rel, outcome_table$agg_source, decreasing = T),]
rel_direct = which(outcome_table_Rel$ID %in% direct)
outcome_table_Edge = outcome_table[order(outcome_table$agg_edge, outcome_table$nConcepts, decreasing = T),]
edge_direct = which(outcome_table_Edge$ID %in% direct)
outcome_table_Source = outcome_table[order(outcome_table$agg_source, outcome_table$agg_rel, decreasing = T),]
source_direct = which(outcome_table_Source$ID %in% direct)
outcome_table_nCon = outcome_table[order(outcome_table$nConcepts, outcome_table$agg_edge, decreasing = T),]
Con_direct = which(outcome_table_nCon$ID %in% direct)
input_table = outcome_table_nCon[1:1000, ]
input_table$direct = F
input_table$direct[Con_direct[Con_direct < 1001]] = T

output = data.frame(Rel = outcome_table_Rel$score, Edge = outcome_table_Edge$score, Source = outcome_table_Source$score, Concept = outcome_table_nCon$score)
#write.csv2(output, "Compounds ranked and scored transmog final filtered 04-11-2016.csv", row.names = F)

output2 = data.frame(Rel = outcome_table_Rel$ID, Edge = outcome_table_Edge$ID, Source = outcome_table_Source$ID, Concept = outcome_table_nCon$ID)
#write.csv2(output2, "Identifiers ranked using various ranking mechanisms.csv", row.names = F)

## Output table -- Calculate costs up to rank k for k 100:1500

results = data.frame(k = seq(from = 100, to = 2000, by = 100))

for(r in 1:nrow(results)){
  results$noweight_Edges[r] = length(which(output$Edge[1:results$k[r]] > 0))
  results$noweight_Edges_avg[r] = round(results$k[r] / length(which(output$Edge[1:results$k[r]] > 0)), 2)
  results$sum_weight_Edges[r] = sum(output$Edge[1:results$k[r]])
  results$weight_Edges[r] = round(sum(output$Edge[1:results$k[r]]) / results$k[r], 2)
  results$noweight_Rels[r] = length(which(output$Rel[1:results$k[r]] > 0))
  results$noweight_Rels_avg[r] = round(results$k[r] / length(which(output$Rel[1:results$k[r]] > 0)), 2)
  results$sum_weight_Rels[r] = sum(output$Rel[1:results$k[r]])
  results$weight_Rels[r] = round(sum(output$Rel[1:results$k[r]]) / results$k[r], 2)
  results$noweight_Source[r] = length(which(output$Source[1:results$k[r]] > 0))
  results$noweight_Source_avg[r] = round(results$k[r] / length(which(output$Source[1:results$k[r]] > 0)), 2)
  results$sum_weight_Source[r] = sum(output$Source[1:results$k[r]])
  results$weight_Source_avg[r] = round(sum(output$Source[1:results$k[r]]) / results$k[r], 2)
  results$noweight_Concept[r] = length(which(output$Concept[1:results$k[r]] > 0))
  results$noweight_Concept_avg[r] = round(results$k[r] / length(which(output$Concept[1:results$k[r]] > 0)), 2)
  results$sum_weight_Concept[r] = sum(output$Concept[1:results$k[r]])
  results$weight_Concept_avg[r] = round(sum(output$Concept[1:results$k[r]]) / results$k[r], 2)
}

#write.csv2(results, "Top compounds statistics transmog final filtered 04-11-2016.csv", row.names = F)

## Create ROC curves

#sum(output$score[1:(nrow(output)/100)]) / sum(output$score)

in_data = outcome_table_nCon

indices = which(in_data$score > 0)

weights = in_data$score[indices]

plotstuff = as.data.frame(matrix(nrow = nrow(in_data), ncol = 3))
colnames(plotstuff) = c("TPR", "weight_TPR", "FPR")

N = nrow(outcome_table_nCon) - length(indices)
for(i in 1:nrow(in_data)){
  TP = length(which(in_data$score[1:i] > 0))
  plotstuff[i, "TPR"] = TP / length(indices) #TPR = TP/P
  plotstuff[i, "weight_TPR"] = sum(in_data$score[1:i]) / sum(in_data$score)
  plotstuff[i, "FPR"] = (i - TP) / (N) #FPR = FP/N
}


### begin insert of corrected code
index = which(outcome_table_nCon$score > 0)

scores = outcome_table_nCon$score[index]

ranker = unique(outcome_table_nCon$nConcepts)

N = length(ranker) - length(unique(outcome_table_nCon$nConcepts[outcome_table_nCon$score > 0]))

plotstuff2 = data.frame()

for(i in 1:length(ranker)){
  print(i)
  TP = length(which(outcome_table_nCon$score[1:max(which(outcome_table_nCon$nConcepts == ranker[i]))] > 0))
  plotstuff2[i, "nCon_TPR"] = TP / length(index) #TPR = TP/P
  plotstuff2[i, "nCon_weight_TPR"] = sum(outcome_table_nCon$score[1:max(which(outcome_table_nCon$nConcepts == ranker[i]))]) / sum(outcome_table_nCon$score)
  pos = length(unique(outcome_table_nCon$nConcepts[which(outcome_table_nCon$score[1:max(which(outcome_table_nCon$nConcepts == ranker[i]))] > 0)]))
  all = length(unique(outcome_table_nCon$nConcepts[1:max(which(outcome_table_nCon$nConcepts == ranker[i]))]))
  plotstuff2[i, "nCon_FPR"] = (all-pos) / N #FPR = FP/N
}


area = c()
for(j in 1:nrow(plotstuff2)){
  addition = plotstuff2$nCon_weight_TPR[j-1] * (plotstuff2$nCon_FPR[j] - plotstuff2$nCon_FPR[j-1])
  area = append(area, addition)
}

ggplot(data = plotstuff2, aes(x = nCon_FPR, y = nCon_TPR, color = "Unweighted performance when ranked based on the number of subgraph concepts")) +
  geom_line(linetype="longdash", size = 1.25) +
  geom_line(data = plotstuff2, aes(x = nCon_FPR, y = nCon_weight_TPR, color = "Weighted performance when ranked based on the number of subgraph concepts"), size = 1.25) +
  labs(x="False Positive Rate",y="True Positive Rate", size) +
  theme(axis.title = element_text(size=30), legend.position = c(0.6, 0.1),  legend.title=element_blank(), legend.text=element_text(size=20)) +
  scale_color_manual(values=c("#6E6E6E", "#190707")) +
  geom_abline(slope = 1, color = "grey")

###### end insert

area = c()
area_weight = c()
for(j in 1:nrow(plotstuff)){
  addition = plotstuff$TPR[j-1] * (plotstuff$FPR[j] - plotstuff$FPR[j-1])
  area = append(area, addition)
  addition_weight = plotstuff$weight_TPR[j-1] * (plotstuff$FPR[j] - plotstuff$FPR[j-1])
  area_weight = append(area_weight, addition_weight)
}
sum(area)
sum(area_weight)

ggplot(data = plotstuff, aes(x = FPR, y = TPR, color = "Unweighted performance")) +
  geom_line(linetype="longdash", size = 2) +
  geom_line(data = plotstuff, aes(x = FPR, y = weight_TPR, color = "Weighted performance"), size = 2) +
  labs(x="False Positive Rate",y="True Positive Rate") +
  theme(axis.title = element_text(size=2), legend.position = c(0.75, 0.08),  legend.title=element_blank(), legend.text=element_text(size=18), axis.text = element_text(size=15)) +
  scale_color_manual(values=c("#6E6E6E", "#190707")) +
  geom_abline(slope = 1, color = "grey", size = 1.5)



# PPV berekenen
output_table = data.frame(percentages = c(0.5,1,2,3,4,5,10,20,25))
output_table$count = round(nrow(output) * (output_table$percentages / 100))
for(x in 1:nrow(output_table)){
  output_table[x, "nPub_specificity"] = round((output_table$count[x] - length(which(output$score[1: output_table$count[x]] > 0))) / nrow(output[output$score == 0, ]), 2) 
  output_table[x, "nPub_weight_rec"] = round(sum(output$score[which(output$score[1: output_table$count[x]] > 0)]) / sum(output$score), 2)
  output_table[x, "nPub_recall"] = round(length(which(output$score[1: output_table$count[x]] > 0)) / length(which(output$score > 0)), 2)
  output_table[x, "nPub_ppv"] = round(length(which(output$score[1: output_table$count[x]] > 0)) / output_table$count[x], 2)
  output_table[x, "nPub_CG"] = round(sum(output$score[1 : output_table$count[x]]) / output_table$count[x], 2)
}

## Create number line
input_values = Con_direct
par(xaxs='i',yaxs='i',mar=c(10,1,1,1))
plot(NA,xlim=c(0, max(input_values)),ylim=c(0,100),axes=F,ann=F)
axis(1)
points(x = input_values, y = rep(0, length(input_values)), pch = 16, xpd = NA)
points(x = nrow(outcome_table_nCon) / 100, y = 0, pch = 15, xpd = NA, col = "red") # 1% van de lijst
points(x = mean(input_values), y = 0, pch = 18, xpd = NA, col = "blue") # Gemiddelde van de direct verbondenen

# Create violin graph
rowcount = 2000
d = outcome_table_nCon[1:rowcount, ]
d$index = 1:rowcount
d = d[which(d$score > 0), ]
d$direct = 0
d$direct[which(d$ID %in% direct)] = d$score[which(d$ID %in% direct)]
d$indirect = abs(d$direct - d$score)
d$list = "Indirect"
d$list[which(d$direct > 0)] = "Direct"

indices_direct = d$index[which(d$direct > 0)]
indices_indirect = d$index[which(d$indirect > 0)]

d$hist_direct = 0
d$hist_indirect = 0
for(i in 1:nrow(d)){
  val = d$index[i]
  if(d$direct[i] > 0){
    hist_data = sum(indices_direct < val + 10 & indices_direct > val - 10)
    d[i, "hist_direct"] = hist_data}
  else{
    hist_data = sum(indices_indirect < val + 10 & indices_indirect > val - 10)
    d[i, "hist_indirect"] = hist_data}
    d[i, "jitter"] = rnorm(1, mean = 0, sd = hist_data/6)
}
d$hist_direct = d$hist_direct * 2.5
d$hist_indirect = d$hist_indirect * 2.5
d$hist_direct = d$hist_direct - (5 * 2.5)
d$hist_indirect = d$hist_indirect - (3 * 2.5)


# Tot aan 1000 doen. Formaat van de punten gelijkstellen aan hun gewicht?
ggplot(data = d, aes(hist_direct, index, fill = list)) + 
  geom_violin(alpha = 0.1) +
  geom_violin(aes(hist_indirect), alpha = 0.1) + 
  geom_point(aes(jitter, index, colour = list, size = score, alpha = 0.75), show.legend = F)  + 
  coord_flip() + 
  geom_point(aes(x=-.2, y = mean(d$index)), colour = "red", size = 3, shape = 17) +
  geom_point(aes(x=.2, y = (nrow(outcome_table) / 100)), colour = "blue", size = 3, shape = 18) +
  scale_x_continuous(breaks=seq(-10,10,5)) +
  theme(legend.position = "none", axis.title = element_text(size=18), axis.text = element_text(size=15)) +  
  ylim(0,2000) + ylab("Rank") +xlab("Density of reference compounds") + xlim(-12, 12)

## Precision & Recall curves

#nCon
indices = which(output$Edge > 0)
indices2 = which(output$Source > 0)
indices3 = which(output$Concept > 0)
indices4 = which(output$Rel > 0)

weights = output$Edge[indices]
weights2 = output$Source[indices2]
weights3 = output$Concept[indices3]
weights4 = output$Rel[indices4]

frame = data.frame(x = 1/length(indices), Edge = 1/indices[1], Edge_weights = weights[1]/indices[1], Source = 1/indices2[1], Source_weights = weights2[1]/indices2[1], Concept = 1/indices3[1], Concept_weights = weights3[1]/indices3[1], Rel = 1/indices4[1], Rel_weights = weights4[1]/indices4[1])

for(i in 2:length(indices)){
  frame[i,"x"] = frame$x[i-1] + 1/length(indices)
  frame[i,"Edge"] = i/indices[i]
  frame[i,"Edge_weights"] = sum(weights[1:i])/indices[i]
  frame[i,"Source"] = i/indices2[i]
  frame[i,"Source_weights"] = sum(weights2[1:i])/indices2[i]
  frame[i,"Concept"] = i/indices3[i]
  frame[i,"Concept_weights"] = sum(weights3[1:i])/indices3[i]
  frame[i,"Rel"] = i/indices4[i]
  frame[i,"Rel_weights"] = sum(weights4[1:i])/indices4[i]
}

# Unweighted plot
#plot(frame$x, frame$y, ylim = c(0,1), type = "l")
ggplot(data = frame, aes(x = x, y = Edge, color = "Ranked based on number of edges to migraine subgraph")) +
  geom_line(linetype="longdash", size = 1.25) + 
  geom_line(aes(y = Source, color = "Ranked based on number of sources to migraine subgraph"), lty = 2, size = 1.25) +
  geom_line(aes(y = Concept, color = "Ranked based on number of concepts to migraine subgraph"), lty = 4, size = 1.25) +
  geom_line(aes(y = Rel, color = "Ranked based on number of relationships to migraine subgraph"), size = 1.25) +
  labs(x="Recall",y="Precision", title = "Precision/Recall graph for the four ranking mechanisms", color = "Ranking Mechanism") + 
  ylim(0,1) +
  theme(plot.title = element_text(size=20, vjust=2), axis.title = element_text(size=15), legend.position = c(0.8, 0.9)) 
+
  scale_color_manual(values=c("#6E6E6E", "#190707"))

# Weighted plot
ggplot(data = frame, aes(x = x, y = Edge_weights, color = "Ranked based on number of edges to migraine subgraph")) +
  geom_line(linetype="longdash", size = 1.25) + 
  geom_line(aes(y = Source_weights, color = "Ranked based on number of sources to migraine subgraph"), lty = 2, size = 1.25) +
  geom_line(aes(y = Concept_weights, color = "Ranked based on number of concepts to migraine subgraph"), lty = 4, size = 1.25) +
  geom_line(aes(y = Rel_weights, color = "Ranked based on number of relationships to migraine subgraph"), size = 1.25) +
  labs(x="Recall",y="Weighted Precision", title = "Weighted Precision/Recall graph for the two ranking mechanisms", color = "Ranking Mechanism") + 
  ylim(0,16) +
  theme(plot.title = element_text(size=20, vjust=2), axis.title = element_text(size=15), legend.position = c(0.8, 0.9)) 
+
  scale_color_manual(values=c("#6E6E6E", "#190707")) +
  scale_y_continuous(breaks = round(seq(0, 13, by = 1),1))