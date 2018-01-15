# Error analysis

neg = which(!all$ID %in% cbackup$ID)
# 15 zitten totaal niet in de lijst
# 11 omdat ze te ver weg zitten
# 2 worden er al in het proces zelf uit gefilterd

sc = which(all$ID %in% filterstuff$V1)
which(sc %in% neg)
# 4 worden eruit gefilterd omdat ze pharma + organic chemical zijn
# 2 worden gefilterd omdat ze alleen via generieke concepten verbonden zijn

lowcon = outcome_table_nCon[which(outcome_table_nCon$nConcepts <= 2), ]
# 37938 candidates hebben slechts 1 of 2 verbindingen met de subgraph
all$ID[which(all$ID %in% lowcon$ID)]
# 14 hebben slechts 1 of 2 verbindingen met de subgraph

midcon = outcome_table_nCon[2001:nrow(outcome_table_nCon), ]
midcon = midcon[-which(midcon$ID %in% lowcon$ID), ]
mid = which(all$ID %in% midcon$ID)
# 24 compounds zitten in de middenmoot --> welke zijn niet altijd negatief?

midcon_Edge = outcome_table_Edge[2001:nrow(outcome_table_Edge), ]
midcon_Edge = midcon_Edge[-which(midcon_Edge$ID %in% lowcon$ID), ]
mid_edge = which(all$ID %in% midcon_Edge$ID)

midcon_Rel = outcome_table_Rel[2001:nrow(outcome_table_Rel), ]
midcon_Rel = midcon_Rel[-which(midcon_Rel$ID %in% lowcon$ID), ]
mid_rel = which(all$ID %in% midcon_Rel$ID)

midcon_Source = outcome_table_Source[2001:nrow(outcome_table_Source), ]
midcon_Source = midcon_Source[-which(midcon_Source$ID %in% lowcon$ID), ]
mid_source = which(all$ID %in% midcon_Source$ID)

sect = Reduce(intersect, list(mid,mid_edge,mid_rel, mid_source))


toofar = c(3685622,
1214612,
1696,
2571528, 
2343610,
888152,
2931530,
2627204,
2928146,
2961780,
2961782)

# Calculate the distribution between the subgraph and other concepts
retrieved = c(1608,2096,2484,2496,2500,2728,2978,3002,3308,3718,4002,4684,4834,4836,5012,5264,5288,6454,6518,7868,7940,7948,9870,10372,11606,11772,11952,12358,14496,15956,15958,16222,16588,17468,17774,17780,18222,18762,19604,19640,19710,19780,20220,21044,21614,21628,21916,22328,22422,22904,22918,22920,22938,23742,23872,23882,23884,23886,23890,23892,23894,24390,24910,25650,25808,25852,26062,26116,26118,26120,26122,26124,26126,26594,26708,26876,26954,27544,27978,30064,30228,30456,30464,30656,30700,31588,32746,33854,34146,34952,35000,35672,36234,36278,36288,36476,36534,37950,40026,40050,40156,40840,41050,42084,42086,42244,42880,43106,43528,43562,43564,44616,44848,45106,45114,45790,46348,47058,75582,75786,79232,80626,93266,94216,102074,103844,104636,109048,109156,120206,129706,130364,130536,138708,161230,162584,163208,164126,165170,221462,223090,224018,227832,329452,412708,457718,463858,713616,818402,824992,825166,854144,939206,989010,1238736,1280572,1328430,1635700,1896914,2010444,2138280,2259834,2280774)
lowcon = c(1696, 18642, 21894, 26138, 34398, 40004, 45914, 60390, 63996, 93328, 162568, 165334, 265966, 706166, 715616, 900144, 1234512, 1240472)

comparison = read.csv2("/Users/Wytze/Documents/PycharmProjects/Neo4j_code/comparison_data.csv", header = F)
low = comparison[which(comparison$V1 %in% lowcon),]
ret = comparison[which(comparison$V1 %in% retrieved),]

for(i in 1:nrow(low)){
  ids = fromJSON(as.character(low$V2[i]))
  low[i, "total length"] = length(ids)
  low[i, "subgraph"] = length(which(ids %in% s$ID))
  low[i, "dist"] = length(ids) / length(which(ids %in% s$ID)) 
}

for(i in 1:nrow(ret)){
  ids = fromJSON(as.character(ret$V2[i]))
  ret[i, "total length"] = length(ids)
  ret[i, "subgraph"] = length(which(ids %in% s$ID))
  ret[i, "dist"] = length(ids) / length(which(ids %in% s$ID)) 
}