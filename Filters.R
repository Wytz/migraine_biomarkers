require(rjson)

## Filter out the general concepts, which are uninformative

General = '{
"Disease" : 14188,
"Chronic Disease" : 10188,
"Rare Diseases" : 908408,
"Body Tissue" : 43826,
"Genes" : 19190,
"Molecular targets" : 2242208,
"Related compounds" : 825890,
"Receptor" : 820372,
"Enzymes" : 16098,
"Vitamins" : 46376,
"Toxin" : 44094,
"Amino acids" : 3622,
"Hormones" : 21946,
"Antigens" : 4550,
"Proteins" : 36588,
"Cytokines" : 103770,
"Pharmaceutical Preparations" : 14706,
"Primary Disease" : 308818,
"Syndrome" : 42644,
"Ions" : 24216,
"Elements" : 15436,
"Steroids" : 41756, 
"Cells" : 8996, 
"DNA" : 14362}'

general_concepts = fromJSON(General)
gframe = t(as.data.frame(general_concepts))
gf = data.frame(Name = rownames(gframe), ID = gframe[,1])
filtergroup = gf
#filtergroup = gf[-c(9, 12, 20),]# "Amino acids" : 3622, # Nodig voor het terugvinden van ref compound 21894
# 16098 (Enzymes) is ook nodig om 1 terug te vinden
# 24216 (Ions) is ook nodig om 1 terug te vinden

#confreq = as.data.frame(table(candidates$Connected_to_ID))
#out = merge(s[, c("Name", "ID")], confreq, by.y = "Var1", by.x = "ID", all.x = T)
#out = out[order(out$Freq, decreasing = T), ]

subgraph = subgraph[-which(subgraph$ID %in% filtergroup$ID),]
candidates = candidates[-which(candidates$Connected_to_ID %in% filtergroup$ID),]
candidates = candidates[-which(candidates$ID %in% filtergroup$ID),]
#non_general = candidates[-which(candidates$Connected_to_ID %in% filtergroup$ID),]

## Filter out the subgraph stuff I don't want
#subgraph_IDs = subgraph[which(subgraph$SemanticTypeIDs == "['Organic Chemicals', 'Pharmacologic Substance']"), "ID"]
#candidates = candidates[-which(candidates$Connected_to_ID %in% subgraph_IDs), ]

## Filter out a lot of concepts, but I lose 6 ref compounds with this
#filterstuff = read.csv2("pharma_curated.txt", header = F)
filterstuff = read.csv2("pharma_not_wanted_2.csv", header = F)
candidates = candidates[-which(candidates$ID %in% filterstuff$V1), ]
candidates = candidates[-which(candidates$Connected_to_ID %in% filterstuff$V1), ]
subgraph = subgraph[-which(subgraph$ID %in% filterstuff$V1), ]

#all_st = unique(c$SemanticTypeIDs)
#incl_st = unique(c$SemanticTypeIDs[which(c$score >0)])
#excl_st = all_st[which(!all_st %in% incl_st)]
#length(unique(c$ID[c$SemanticTypeIDs %in% excl_st]))

# Filter out the negative relationships <-- Verslechteren performance een klein beetje
#pred_mapped = read.csv2("Predicates_mapped.csv", header = F, stringsAsFactors = F)
#colnames(pred_mapped) = c("Pred_ID", "Label", "Category")
#ids = pred_mapped$Pred_ID[pred_mapped$Category == "neg"]
#subgraph = subgraph[-which(subgraph$PredicateID %in% ids), ]
#candidates = candidates[-which(candidates$PredicateID %in% ids), ]
#candidates = candidates[which(candidates$Connected_to_ID %in% subgraph$ID), ] # Je verliest een paar subgraph ID's, dus die mogen ook niet meer meedoen met de identificatie van biomarkers

## Filter out species which are irrelevant (Optional step)
#names_list = as.character(unique(candidates$Name))
#species = unlist(strsplit(names_list, ", "))
#species_frame = as.data.frame(table(species))
#species_frame = species_frame[order(species_frame$Freq, decreasing = T),]

grepperdegrep = c(", E coli", ", Bacterial", ", Viral", ", S pombe", ", bacteria", ", Xenopus", ", Synthetic", ", Arabidopsis", ", Zea mays", ", C elegans", ", S cerevisiae", ", Drosophila", ", zebrafish", ", Klebsiella pneumoniae") #Rat moet erin blijven, want daar heb ik een compound naar gemapt
for(i in 1:length(grepperdegrep)){
  print(grepperdegrep[i])
  print(length(grep(grepperdegrep[i], outcome_table$Name)))
  outcome_table = outcome_table[-grep(grepperdegrep[i], outcome_table$Name), ]
}

## Error analysis for filtering
#pharmas = all$ID[which(all$ID %in% filterstuff$V1)]
#all[which(all$ID %in% pharmas[which(pharmas %in% candidates$ID)]),]

# Deze verlies je omdat ze geen positieve relaties hebben met de subgraph
[1] "Ammonia"                                    "Antithrombin III"                          

[1] "['Inorganic Chemical', 'Pharmacologic Substance']"              "['Pharmacologic Substance', 'Amino Acid, Peptide, or Protein']"