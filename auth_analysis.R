# Script to measure the number of citations per author
library(data.table)
# Import profiles
df = fread("author_level_citations_papers.csv", data.table=F)
x = sort(df$n_citations)
plot(x, type="l")

pmids = read.table("ad_mesh.txt")[,1]

# Measure citations per author as fraction of total AD literature
y = x / length(pmids)
plot(y, type="l")

mean(y)
# 0.00267374

# Measure per author as fraction of total AD literature for top authors
z = y[(length(y) - 149):length(y)]
plot(z, type="l")
mean(z)
# 0.07832564

write.csv(y, "author_coverage.csv", row.names=F)
write.csv(z, "author_coverage_150.csv", row.names=F)
