# importation de spackage por la manipulation des données-----
 library(tidyverse)

# DAATAAAAAAATTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAA
data = read.csv("data_Thesis_I.csv")
data |> str()#structure
data |> skimr::skim()
data |> dlookr::normality()

## COMMMMMEEENNNNTTTTT PROCEDONS NOUS ALORS??????
### Abondance et divrsite globales (ecosystem)
### Abondance et indices pour chaque unités


#First step we split the data with differents AP
mar = data |> filter(ap == "Mariarano")
ank = data |> filter(ap == "Ankarafantsika")
bom = data |> filter(ap == "Bombetoka")


#Next, we compute the total number of species for each AP
mar.ab = table(mar$site,mar$sp) |> as.data.frame.matrix()
ank.ab = table(ank$site,ank$sp) |> as.data.frame.matrix()
bom.ab = table(bom$site,bom$sp)|> as.data.frame.matrix()

clustering_Ap <- function(data.ab,normalization,dissimilarity,clustering) {
          res.norm = vegan::decostand(data.ab,method = normalization);
          res.dist = vegan::vegdist(res.norm,method = dissimilarity);
          res.clustering =hclust(res.dist,method = clustering);
  #Diversity index
    shannon_index <- vegan::diversity(data.ab, index = "shannon")
    simpson_index <- vegan::diversity(data.ab, index = "simpson")
    species_richness <- vegan::specnumber(data.ab)
    
    # Plot the clustering dendrogram
    plot(res.clustering, hang = -1)
    
    # Return results as a list
    return(list(clustering = res.clustering, 
                shannon_index = shannon_index, 
                simpson_index = simpson_index, 
                species_richness = species_richness))
}
clustering_Ap(data.ab = mar.ab,normalization = "hellinger",
                    dissimilarity = "horn",
                  clustering = "ward.D2")


  
