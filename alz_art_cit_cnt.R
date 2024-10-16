# Script to measure the number of citations to AD adticles
library(data.table)
library(fastmatch)

# Load and transform data
occ = fread("ad_mesh_occ.csv", data.table = F)
x=tapply(occ$citing, occ$referenced, length)
df = data.frame("pmid"=as.integer(names(x)), "n_citations"=x)
write.csv(df, "ad_mesh_cit_count.csv", na="")

ad_mesh = read.table("ad_mesh.txt", header=F)[,1]
summary(fmatch(ad_mesh, ad_mesh))

# Function to restructure citation data to wide form
gen_reflist = function(occ) {
  refs_ls = tapply(occ$citing, occ$referenced, c, simplify=F)
  refs_index = as.integer(names(refs_ls))
  summary(fmatch(refs_index, refs_index))
  references = list("index"=refs_index, "references"=refs_ls)
  #summary(fmatch(references$index, references$index))
  return(references)
}

# Restructure using custom function
reflist = gen_reflist(occ)
reflist$references = sapply(reflist$references, function(a) paste(a, collapse=" "))
df = as.data.frame(reflist)
colnames(df) = c("citing", "referenced")

# Save
write.csv(df, "ad_mesh_reflist.csv", 
          row.names=F, na="")

# Compare AD dataset to non-AD dataset
nihocc = fread("open_citation_collection.csv", data.table=F)

ad_occ_super = nihocc[nihocc[,2] %fin% ad_mesh,]

# Non-AD citations to AD articles
nrow(ad_occ_super[!(ad_occ_super[,1] %fin% ad_mesh),])
# 2643076

# AD citations to AD articles
nrow(occ)
# 2051603

# Total
nrow(ad_occ_super)
# 4694679
