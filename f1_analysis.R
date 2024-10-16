# Script to calculate author level measures
library(data.table)

# Load data
ad_mesh_icite = fread("ad_mesh_icite.csv", data.table=F)
ad_mesh_auth = fread("author_mesh_profiles.csv", data.table=F)

# Papers per year overall
ppy = tapply(ad_mesh_icite$year, ad_mesh_icite$year, length)
ppy_df = data.frame("year"=names(ppy[as.character(1980:2019)]), "n"=ppy[as.character(1980:2019)])
plot(ppy_df$year, ppy_df$n)

# Papers per year by author
apy = tapply(ad_mesh_auth$AND_ID, ad_mesh_auth$PubYear, function(x) length(unique(x)))
apy_df = data.frame("year"=names(apy[as.character(1980:2019)]), "n"=apy[as.character(1980:2019)])
plot(apy_df$year, apy_df$n)

# Save
write.csv(ppy_df, "ppy_df.csv", row.names=F)
write.csv(apy_df, "apy_df.csv", row.names=F)
