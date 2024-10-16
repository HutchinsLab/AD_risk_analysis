# Script to cluster citations for data visualiztion
library(data.table)
library(fastmatch)
source("citeIDs.R")

# Function to remap citations into a format compatable with the clustering algorithm
remap_ids = function(occ,  remove_self_loops=T, sep="\r", count=0) {
  occ = as.data.frame(occ)
  colnames(occ)[1:2] = c("citing", "referenced")
  occ$atleast = occ$citing >= occ$referenced
  
  if (typeof(occ$citing) == "integer" && typeof(occ$citing) == "integer") {
    m = matrix(nrow=nrow(occ), ncol=2, data=as.integer(NA))
  } else {
    m = matrix(nrow=nrow(occ), ncol=2, data=as.character(NA))
  }
  colnames(m)[1:2] = c("citing", "referenced")
  m[occ$atleast,"citing"] = occ$citing[occ$atleast]
  m[!occ$atleast,"referenced"] = occ$citing[!occ$atleast]
  m[!occ$atleast,"citing"] = occ$referenced[!occ$atleast]
  m[occ$atleast,"referenced"] = occ$referenced[occ$atleast]
  
  if (typeof(m) == "integer") {
    m = m[!duplicated(citeID(m[,"citing"], m[,"referenced"]), F),]
  } else {
    m = m[!duplicated(paste(m[,"citing"], m[,"referenced"], sep="\r")),]
  }
  
  if (remove_self_loops) m = m[m[,"citing"] != m[,"referenced"],]
  
  m = na.omit(m)
  
  ids = unique(c(m[,"citing"], m[,"referenced"]))
  
  occ_map = data.frame("orig_id"=ids, "new_id"=1:length(ids))
  if (count == 0) occ_map$new_id = occ_map$new_id - 1
  
  #n_nodes = length(ids)
  #n_edges = length(unique(citeID(m[,"citing"], m[,"referenced"], F)))
  
  summary(fmatch(occ_map$orig_id, occ_map$orig_id))
  summary(fmatch(occ_map$new_id, occ_map$new_id))
  
  occ2 = data.frame("id_citing"=occ_map$new_id[fmatch(m[,"citing"], occ_map$orig_id)],
                    "id_referenced"=occ_map$new_id[fmatch(m[,"referenced"], occ_map$orig_id)])
  
  occ2$id_citing = as.integer(occ2$id_citing)
  occ2$id_referenced = as.integer(occ2$id_referenced)
  
  return(list("citelinks"=occ2, "id_map"=occ_map))
}

# Export function for the clustered papers
write_unweighted = function(occ, path, idmap, format="leiden") {
  if (format == "leiden") {
    if (typeof(occ[,1]) != "integer" | typeof(occ[,2]) != "integer") stop("Leiden format requires integers.")
    fwrite(occ, paste0(path, "leiden_format.tsv"), sep="\t", col.names = F)
    fwrite(idmap, paste0(path, "leiden_id_map.csv"))
  } else if (format == "metis") {
    
  } else {
    
  }
}

# Define path here
path = ""

# Load citation data for clustering
occ = fread(paste0(path, "ad_mesh_occ.csv"), data.table=F)

# Remap and write out
data = remap_ids(occ)

fwrite(data$citelinks, paste0(path, "leiden_format.tsv"), sep="\t", col.names = F)
fwrite(data$id_map, paste0(path, "leiden_id_map.csv"))

# Execute shell script to run Java clustering as cited in Methods
cmd_prefix = "java -Xmx250g -jar "
params =" --resolution 0.005 "
leiden_loc = "RunNetworkClustering.jar"

cmd = paste0(cmd_prefix, leiden_loc, params,
             paste0(path, "leiden_format.tsv"),
             " > ", paste0(path, "leiden_clusters.tsv"))

shell(cmd, wait=T, mustWork=T)

# Read clusters
cl = fread(paste0(path, "leiden_clusters.tsv"))
names(cl) = c("id", "cl")

ids = fread(paste0(path,  "\\leiden_id_map.csv"), data.table=F)
summary(fmatch(ids$new_id, ids$new_id))

# Process clusters and save as table
cl_count = sort(table(cl[,2]), decreasing=T)
cl_df = data.frame("new_cl"=0:(length(cl_count)-1), "old_cl"=as.integer(names(cl_count)))
summary(fmatch(cl_df$old_cl, cl_df$old_cl))
summary(cl$id %fin% ids$new_id)
cl$id = ids$orig_id[fmatch(cl$id, ids$new_id)]
cl$cl = cl_df$new_cl[fmatch(cl$cl, cl_df$old_cl)]

summary(factor(cl$cl))

cl_txt = tapply(cl$id, cl$cl, function(x) paste(x, collapse = " "))
nm = names(cl_txt)
cl_txt = as.vector(cl_txt)
names(cl_txt) = nm
cl_txt = cl_txt[order]
options(scippen=999)
fwrite(cl, paste0(path,  "\\leiden_cluster_table.tsv"), sep="\t")
write.table(data.frame(cl_txt), paste0(path,  "\\leiden_clusters.txt"), col.names=F, row.names=F, quote=F)

# Link citations between clusters by citation count and save
occ$citing_cl = cl$cl[fmatch(occ$citing, cl$id)]
occ$referenced_cl = cl$cl[fmatch(occ$referenced, cl$id)]

cluster_citations = paste(occ$citing_cl, occ$referenced_cl, sep="_")
cluster_citations_agg = tapply(cluster_citations, cluster_citations, length)
nm2 = names(cluster_citations_agg)
cluster_citations_agg = as.vector(cluster_citations_agg)
names(cluster_citations_agg) = nm2
cluster_citation_df = na.omit(data.frame("citing_cl" = as.integer(tstrsplit(names(cluster_citations_agg), "_", fixed=T)[[1]]),
                                 "referenced_cl" = as.integer(tstrsplit(names(cluster_citations_agg), "_", fixed=T)[[2]]),
                                 "weight"=cluster_citations_agg))
fwrite(cluster_citation_df, paste0(path, "cluster_citations.csv"), row.names = F)

# Exclude very small clusters and save
x = table(cl$cl)
x = sort(x, decreasing = T)
y = cumsum(x)
plot(y/max(y), type="l")

cl_size = data.frame("cluster"=as.integer(names(x)),
                     "size"=as.integer(x))

fwrite(cl_size, paste0(path, "cluster_size.csv"))

trimmed_edges = cluster_citation_df[cluster_citation_df$citing_cl %in% cl_size$cluster[cl_size$size >= 5] &
                                      cluster_citation_df$referenced_cl %in% cl_size$cluster[cl_size$size >= 5],]
fwrite(trimmed_edges, paste0(path, "trimmed_cluster_citations.csv"))
