# Script to calculate citation graph distances
library(data.table)
library(igraph)
occ = read.csv("ad_mesh_occ.csv", stringsAsFactors = F)
pmids = unique(c(occ[,1], occ[,2]))
# Renumber starting at 1 and going to n
ids = 1:length(pmids)
idf = data.frame(pmids, ids)

occid = data.frame("citing"=idf$ids[match(occ$citing, idf$pmids)],
                   "referenced"=idf$ids[match(occ$referenced, idf$pmids)])

# Munge into igraph-compatable data structure
g = make_graph(as.matrix(occid))
trans = transitivity(g, "average")
cl = clusters(g)

lcc_nodes = ids[cl$membership == 1]
occid2 = occid[occid$citing %in% lcc_nodes & occid$referenced %in% lcc_nodes,]
g2 = make_graph(as.matrix(occid2))
m2 = mean_distance(g2)

g3 = make_full_citation_graph(length(ids))

# Calculate citation distance table
dt = distance_table(g, directed = T)
hist(dt$res)

# Save
write.csv(data.frame("Distance"=dt$res), "distance_table.csv", row.names = F)
