# Script to build datasets
library(data.table)
library(fastmatch)
#This script no longer works because PubMed changed their API
source("query_pubmed_old.R")

# Query PubMed with Alzheimer Disease MeSH terms or 'alzheimer' in the title
q = "(Alzheimer Disease[mh]) OR alzheimer[title]"
pmids = query_pubmed(q, 1000000)
write.table(pmids, "ad_mesh.txt", row.names=F, col.names=F, quote=F)

# This dataset was tested but not used in the paper because of data quality issues
ad_rcdc = NULL
ad_rcdc = rbind(ad_rcdc, fread("SearchResult_Export_02Aug2022_110352.csv", data.table=F, skip=6))
ad_rcdc = rbind(ad_rcdc, fread("SearchResult_Export_02Aug2022_110607.csv", data.table=F, skip=6))
ad_rcdc = rbind(ad_rcdc, fread("SearchResult_Export_02Aug2022_110751.csv", data.table=F, skip=6))
core = paste0(ad_rcdc$Activity, ad_rcdc$IC, ad_rcdc$`Serial Number`)
table(nchar(core))
core = unique(core[nchar(core) > 10])
# End unused code

# Query that was used in the paper
# 08/08/2022 "(Alzheimer Disease[mh]) OR alzheimer[title]"
ad_mesh = read.table("ad_mesh.txt", header=F)[,1]

# Also unused but was tested during data gathering
spires = NULL
for (y in 1980:2020) {
  print(y)
  spires = rbind(spires, fread(paste0("RePORTER_PUBLNK_C_", y, ".csv"), data.table=F))
}
nih_pmids = unique(spires$PMID)
summary(fmatch(nih_pmids, nih_pmids))
summary(fmatch(spires$PMID, spires$PMID))
summary(fmatch(spires$PROJECT_NUMBER, spires$PROJECT_NUMBER))
# R00, R01, R03, R15, R21, R33, R34, R35, R36, R37, R50, R56, R61, RC1, RC2, RC3, RC4, RF1, RL1, RL2, RL9, P01, P42, PM1, PN1, RM1, UA5, UC1, UC2, UC3, UC4, UC7, UF1, UG3, UH2, UH3, UH5, UM1, UM2, U01, U19, U34, U3R, DP1, DP2, DP3, DP4, DP5
# RPG def 2022 https://grants.nih.gov/grants/glossary.htm
rpg = "R00, R01, R03, R15, R21, R33, R34, R35, R36, R37, R50, R56, R61, RC1, RC2, RC3, RC4, RF1, RL1, RL2, RL9, P01, P42, PM1, PN1, RM1, UA5, UC1, UC2, UC3, UC4, UC7, UF1, UG3, UH2, UH3, UH5, UM1, UM2, U01, U19, U34, U3R, DP1, DP2, DP3, DP4, DP5"
rpg = strsplit(rpg, ", ", fixed=T)[[1]]

ad_spires = spires$PMID[spires$PROJECT_NUMBER %in% core]

spires_ad = spires[spires$PROJECT_NUMBER %in% core,]
summary(factor(substr(spires_ad$PROJECT_NUMBER, 1, 3)))
spires_rpg = spires_ad[substr(spires_ad$PROJECT_NUMBER, 1, 3) %in% rpg,]

combined_alz_pmids = unique(c(ad_mesh, spires_rpg$PMID))

write.table(combined_alz_pmids, "combined_alz_pmids.txt", row.names=F, col.names=F)

summary(fmatch(combined_alz_pmids, combined_alz_pmids))
# End unused code

# Read author profiles as cited in the Methods
auth = fread("OA01_Author_List.csv", data.table=F)

# Also tested but unused because of data quality problems
ad_combined_auth = auth[auth$PMID %fin% combined_alz_pmids,]

write.csv(ad_combined_auth, "author_combined_profiles.csv", row.names=F)
# End unused code

# Data pulled from the iCite API for the 2021-06 dataset
icite = fread("uber_icite.tsv", data.table=F)
occ = fread("open_citation_collection.csv", data.table=F)

# Unused
ad_combined_occ = occ[occ[,1] %fin% combined_alz_pmids & occ[,2] %fin% combined_alz_pmids,]
ad_combined_icite = icite[icite$pmid %fin% combined_alz_pmids,]

write.csv(ad_combined_icite, "ad_combined_icite.csv", row.names=F)
write.csv(ad_combined_occ, "ad_combined_occ.csv", row.names=F)
# End unused

# This is the dataset that is used in the paper
# Just AD MeSH terms or alzheimer in the title
ad_mesh_occ = occ[occ[,1] %fin% ad_mesh & occ[,2] %fin% ad_mesh,]
ad_mesh_icite = icite[icite$pmid %fin% ad_mesh,]

write.csv(ad_mesh_icite, "ad_mesh_icite.csv", row.names=F)
write.csv(ad_mesh_occ, "ad_mesh_occ.csv", row.names=F)

ad_mesh_auth = auth[auth$PMID %fin% ad_mesh,]
write.csv(ad_mesh_auth, "author_mesh_profiles.csv", row.names=F)
