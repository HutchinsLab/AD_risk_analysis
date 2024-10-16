# Script to measure the number of AD papers and citations
# and filter to the criteria lined out in the Methods section
library(data.table)
library(fastmatch)

# Load data
occ = read.csv("ad_mesh_occ.csv", stringsAsFactors = F)
summary(fmatch(occ[,1], occ[,1]))
summary(fmatch(occ[,2], occ[,2]))
auth = read.csv("author_mesh_profiles.csv", stringsAsFactors = F)
ad_mesh = read.csv("ad_mesh.txt", stringsAsFactors = F, header=F)[,1]

# Index first- or last-author papers
auth_fl = auth[auth$AuOrder == 1 | auth$AuOrder == auth$AuNum,]

authors = unique(auth_fl$AND_ID)

sort(table(auth_fl$AND_ID), decreasing = T)

hist(table(auth_fl$AND_ID)[names(table(auth_fl$AND_ID)) != "0"], breaks=200, xlim=c(0,10))

two_or_more = names(table(auth_fl$AND_ID)[table(auth_fl$AND_ID) > 1])
two_or_more = as.integer(two_or_more[two_or_more != "0"])

df = data.frame("author"=two_or_more, "n_papers" = NA_integer_, "n_citations" = NA_integer_)

# Measure citations and papers for AD authors
for (i in 1:length(two_or_more)) {
  p = unique(auth$PMID[auth$AND_ID == df[i,"author"]])
  if (length(p) < 1) {
    df[i, "n_papers"] = 0
    df[i, "n_citations"] = 0
    next
  }
  df[i,"n_papers"] = length(p)
  df[i, "n_citations"] = length(unique(occ[occ[,2] %in% p, 1]))
  if (i %% 1000 == 0) print(i/length(two_or_more))
  if (i == 500) print(i/length(two_or_more))
  if (i == 100) print(i/length(two_or_more))
  if (i == 50) print(i/length(two_or_more))
  if (i == 10) print(i/length(two_or_more))
}

x = sort(df$n_citations)
plot(x, type="l")

# Save data
write.csv(df, "author_level_citations_papers.csv", row.names=F)

df = df[order(df$n_citations, decreasing = T),]

highly_cited = df$author[1:150]

# Measure highly cited papers
highly_cited_papers_dup = sum(df$n_papers[1:150])
# 27226
highly_cited_pmids = unique(auth$PMID[auth$AND_ID %in% highly_cited])
highly_cited_papers_dedup = length(highly_cited_pmids)
# 16864 
highly_cited_papers_dedup / highly_cited_papers_dup
# 0.6194079
highly_cited_papers_dedup / length(ad_mesh)

# Save
auth_not_zero = auth[auth$AND_ID != 0,]
npapers = tapply(auth_not_zero$PMID, auth_not_zero$AND_ID, length)
npapers = sort(npapers, decreasing=T)
npapers_all_df = data.frame("auth"=as.integer(names(npapers)), "n_papers"=npapers)
write.csv(npapers_all_df, "author_level_papers_nofilter.csv", row.names=F, na="")
