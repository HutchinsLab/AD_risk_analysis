# Script to compare references also cited by other references from the same paper
# using the real data vs. data where citations were randomly shuffled

# Load occ
# Load incites
# compare
# Shuffle citations by year
# save
# recalc incites
# load
# compare

library(data.table)
library(fastmatch)
source("citeids.r")
# Load data
occ = read.csv("ad_mesh_occ.csv", stringsAsFactors = F)
icite = fread("uber_icite.tsv", data.table=F)
# Load non-shuffled data
incites = fread("incites.tsv", data.table=F)

summary(fmatch(icite$pmid, icite$pmid))

incites_id = citeID(incites[,1], incites[,2], cast=F)
summary(fmatch(incites_id, incites_id))

occ_id = citeID(occ[,1], occ[,2])
summary(fmatch(occ_id, occ_id))

# Measue references also cited by other references from the same paper
# with non-shuffled data
nrow(occ)
summary(occ_id %fin% incites_id)
#Mode   FALSE    TRUE 
#logical  716088 1335515
#         0.3490 0.65096

occ$citing_year = icite$year[fmatch(occ$citing, icite$pmid)]
occ$referenced_year = icite$year[fmatch(occ$referenced, icite$pmid)]

ys = sort(unique(occ$citing_year))

tapply(occ$citing, occ$citing_year, length)

# Shuffle data
occ_shuf = occ
for (y in ys) {
  datasub = occ[occ$citing_year == y, "referenced"]
  if (length(datasub) > 1) {
    temp = sample(datasub)
  } else {
    temp = datasub
  }
  print(length(temp) == length(occ[occ$citing_year == y, "referenced"]))
  occ[occ$citing_year == y, "referenced"] = temp
  print(y)
}

occ_shuf_citing = tapply(occ_shuf$referenced, occ_shuf$citing, paste, collapse=" ")

df_shuf = data.frame("citing"=as.integer(names(occ_shuf_citing)), "referenced" = occ_shuf_citing)

write.table(df_shuf, "citations_shuffled.tsv", row.names=F, col.names=T, sep="\t", na="")
write.csv(occ_shuf, "occ_shuf_by_year.csv", row.names=F, na="")

# run incites.py here on shuffled data

# read output
incites_shuf = fread("citations_shuffled_incites.tsv", data.table=F)

occ_shuf_id = citeID(occ_shuf[,1], occ_shuf[,2])

incites_shuf_id = citeID(incites_shuf[,1], incites_shuf[,2], cast=F)

summary(occ_shuf_id %in% incites_shuf_id)
#Mode   FALSE    TRUE 
#logical  847457 1204146
#         0.4131 0.5869

# Test significance of differences between shuffled and non-shuffled data
m = rbind(c(1335515, 716088), c(1204146, 847457))
fisher.test(m)
#Fisher's Exact Test for Count Data
#
#data:  m
#p-value < 2.2e-16
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
# 1.307350 1.317849
#sample estimates:
#odds ratio 
#   1.31255 

