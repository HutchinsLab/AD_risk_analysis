# Script for exploratory data analysis on cluster statistics
library(data.table)
library(fastmatch)

# Import data
icite = fread("ad_mesh_icite.csv", data.table=F)
summary(fmatch(icite$pmid, icite$pmid))

# Detect which papers were NIH funded
# Not used in the paper in the end
spires = NULL
for (year in 1980:2020) {
  spires = rbind(spires, fread(paste0("./RePORTER_PUBLNK_C_", year, ".csv"), data.table=F))
}
summary(fmatch(spires$PMID, spires$PMID))

icite$nih = icite$pmid %fin% spires$PMID
# End unused

# Load clusters
cl = fread("leiden_cluster_table.tsv", data.table=F)
summary(fmatch(cl$id, cl$id))

size = fread("cluster_size.csv", data.table=F)

# Load author table
auth = fread("author_mesh_profiles.csv", data.table=F)

# Load citations
occ = fread("ad_mesh_occ.csv", data.table=F)

# Filter small clusters
valid_clusters = size$cluster[size$size >= 5]

size = size[size$size >= 5,]

# ~2% authors: 9295037, 2209001, 6270170, 4914140
# top author: 4920899
# random from top 250: 5556595, 196513

au1 = unique(auth$PMID[auth$AND_ID == 4914140])
au2 = unique(auth$PMID[auth$AND_ID == 9295037])
au3 = unique(auth$PMID[auth$AND_ID == 2209001])
au4 = unique(auth$PMID[auth$AND_ID == 6270170])
au5 = unique(auth$PMID[auth$AND_ID == 4920899])
au6 = unique(auth$PMID[auth$AND_ID == 5556595])
au7 = unique(auth$PMID[auth$AND_ID == 196513])

# Build features for exploratory data analysis
i = 0
for (clnum in size$cluster) {
  i = i + 1
  if (i %% 50 == 0) print(i/nrow(size))
  fields = c("avg_citations", "avg_cpy", "avg_rcr",
             "avg_authors", "avg_career_age", "human", "animal", "molcell",
             "pct_nih", "pct_clin", "au1", "au2", "au3", "au4", "au5", "au6", "au7")
  
  # Loop over fields and calc stats
  
  pmids = cl$id[cl$cl == clnum]
  
  size[size$cluster == clnum, "avg_citations"] =
    mean(icite$citation_count[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "avg_cpy"] =
    mean(icite$citations_per_year[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "avg_rcr"] =
    mean(icite$relative_citation_ratio[icite$pmid %in% pmids], na.rm=T)
  auth_subset = auth[auth$PMID %in% pmids,]
  auth_subset_num = auth_subset[!duplicated(auth_subset$PMID),]
  auth_subset_au = auth_subset[!duplicated(auth_subset$AND_ID),]
  size[size$cluster == clnum, "avg_authors"] = mean(auth_subset_num$AuNum, na.rm=T)
  size[size$cluster == clnum, "avg_career_age"] = 2021 - mean(auth_subset_au$BeginYear, na.rm=T)
  size[size$cluster == clnum, "human"] =
    mean(icite$human[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "animal"] =
    mean(icite$animal[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "molcell"] =
    mean(icite$molecular_cellular[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "pct_nih"] =
    mean(icite$nih[icite$pmid %in% pmids], na.rm=T)
  size[size$cluster == clnum, "pct_clin"] =
    mean(icite$is_clinical[icite$pmid %in% pmids] == "Yes", na.rm=T)
  size[size$cluster == clnum, "au1"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au1], na.rm=T)
  size[size$cluster == clnum, "au2"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au2], na.rm=T)
  size[size$cluster == clnum, "au3"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au3], na.rm=T)
  size[size$cluster == clnum, "au4"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au4], na.rm=T)
  size[size$cluster == clnum, "au5"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au5], na.rm=T)
  size[size$cluster == clnum, "au6"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au6], na.rm=T)
  size[size$cluster == clnum, "au7"] =
    mean(pmids %in% occ$citing[occ$referenced %in% au7], na.rm=T)
}

# Export to csv
fwrite(size, "cluster_statistics.csv")

