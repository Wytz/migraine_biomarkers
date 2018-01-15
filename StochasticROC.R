# Create stochastic ROC curve

input = outcome_table_nCon
ROC_out = data.frame(x = seq(1,100,0.5))

positives = input[which(input$score > 0),]
negatives = input[which(input$score == 0),]

ROC_values = data.frame(x = seq(1,100,0.5))

# Sample ranking values from dataset to use in ROC plot
for(i in 1:nrow(positives)){
  ROC_temp = data.frame(x = seq(1,100,0.5))
  posranks = sample(nrow(positives), 1)
  positives = positives[-posranks, ]
  negranks = sample(nrow(negatives), 99)
  measurement = data.frame(class = "positive", value = positives$nConcepts[posranks])
  measurement = rbind(measurement, data.frame(class = "negative", value = negatives$nConcepts[negranks]))
  measurement$value = rank(-measurement$value, ties.method = "average")
  breakpoint = measurement$value[measurement$class == "positive"]
  ROC_temp$y = 0
  ROC_temp$y[which(ROC_values$x == breakpoint):nrow(ROC_values)] = 1
  ROC_values = cbind(ROC_values, ROC_temp$y)
}

ROC_values$y_coords = rowSums(ROC_values[,-1])/nrow(input[which(input$score > 0),])
unweighted_AUC = sum(ROC_values$y_coords)/200

ROC_out = cbind(ROC_out, unweighted = ROC_values$y_coords)

#============================ Weighted code ===============================

positives = input[which(input$score > 0),]
weighted_positives = data.frame()
for(p in 1:nrow(positives)){
  temp = data.frame(nConcepts = rep(positives$nConcepts[p], times = positives$score[p]), score = rep(1, times = positives$score[p]))
  weighted_positives = rbind(weighted_positives, temp)
}

ROC_values = data.frame(x = seq(1,100,0.5))

for(i in 1:nrow(weighted_positives)){
  ROC_temp = data.frame(x = seq(1,100,0.5))
  posranks = sample(nrow(weighted_positives), 1)
  weighted_positives = weighted_positives[-posranks, ]
  negranks = sample(nrow(negatives), 99)
  measurement = data.frame(class = "positive", value = weighted_positives$nConcepts[posranks])
  measurement = rbind(measurement, data.frame(class = "negative", value = negatives$nConcepts[negranks]))
  measurement$value = rank(-measurement$value, ties.method = "average")
  breakpoint = measurement$value[measurement$class == "positive"]
  ROC_temp$y = 0
  ROC_temp$y[which(ROC_values$x == breakpoint):nrow(ROC_values)] = 1
  ROC_values = cbind(ROC_values, ROC_temp$y)
}

ROC_values$y_coords = rowSums(ROC_values[,-1])/sum(input$score)
weighted_AUC = sum(ROC_values$y_coords)/200

ROC_out = cbind(ROC_out, weighted = ROC_values$y_coords)
ROC_out$x = ROC_out$x/100
ROC_out = rbind(ROC_out, c(0,0,0))
# Create ROC plot
require(ggplot2)
ggplot(data = ROC_out, aes(x = x, y = unweighted, color = "Unweighted performance when ranked based on the number of subgraph concepts")) +
  geom_line(linetype="longdash", size = 1.25) +
  geom_line(data = ROC_out, aes(x = x, y = weighted, color = "Weighted performance when ranked based on the number of subgraph concepts"), size = 1.25) +
  labs(x="False Positive Rate",y="True Positive Rate") +
  theme(axis.title = element_text(size=15), legend.position = c(0.6, 0.1),  legend.title=element_blank()) +
  scale_color_manual(values=c("#6E6E6E", "#190707")) +
  geom_abline(slope = 1, color = "grey") 
