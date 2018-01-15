#!/usr/bin/Rscript
require(data.table)

## Load the data, and create tables of unique entities
# Subgraph
subgraph = read.csv2("Adjacent connections 21-10-2016.csv", stringsAsFactors = F, header = F)
colnames(subgraph) = c("Name", "ID", "PredicateID", "SemanticTypeIDs", "nSources", "Sources")

# Candidate compounds
candidates = read.csv2("Indirect_connections 21-10-2016.csv", stringsAsFactors = F, header = F)
colnames(candidates) = c("Connected_to_ID", "Name", "ID", "PredicateID", "SemanticTypeIDs", "nSources", "Sources")

# Zijn per ongeluk nog een boel drugs in de indirecte lijst gekomen, die geen verbinding met de adjacent lijst hebben.
# Die moeten (en mogen) eruit gefilterd worden
candidates = candidates[which(candidates$Connected_to_ID %in% subgraph$ID),]
cbackup = candidates

## Load the reference set to evaluate the results
selected = read.csv("/Users/Wytze/Dropbox/Scientific Research Project/Results/Updated compound lists November 2014/Definite_CSF_compounds_migraine_node_IDs_16-01-2015.csv", stringsAsFactors = F, header = T, sep = ";")

selected_serum = read.csv("/Users/Wytze/Dropbox/Scientific Research Project/Results/Updated compound lists November 2014/Definite_serum_migraine_node_ID_16-01-2015_unique.csv", stringsAsFactors = F, header = F, sep = ";")
colnames(selected_serum) = c("Compound", "UMLS", "ID", "Lit_count", "pref_scheme", "all_scheme")
all = merge(selected, selected_serum, by = "ID", all = T)

all[is.na(all)] = 0
all$total = all$Selected + all$Lit_count

# Identify which reference compounds also have a direct connection
direct = all$ID[which(all$ID %in% subgraph$ID)]
