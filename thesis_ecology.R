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


#################### Analayse floristique##############
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
  #Diversity index (indices biocénotique)
    # shannon best value : 3-4>   
    shannon_index <- vegan::diversity(data.ab, index = "shannon")
    simpson_index <- vegan::diversity(data.ab, index = "simpson")
    species_richness <- vegan::specnumber(data.ab)
    #Pielou best value : 0.8
    pielou_index<- shannon_index/log2(species_richness)
    
    # Plot the clustering dendrogram
    plot(res.clustering,hang=-1)
    
    # Return results as a list
    return(list(clustering = res.clustering, 
                shannon_index = shannon_index, 
                simpson_index = simpson_index, 
                species_richness = species_richness,
              pielou_index = pielou_index)
            )
}
#Mariarano biodiversity
clustering_Ap(data.ab = mar.ab,normalization = "hellinger",
                    dissimilarity = "horn",
                  clustering = "ward.D2")
#Ankarafantsika biodiversity
clustering_Ap(data.ab = ank.ab,normalization = "hellinger",
                    dissimilarity = "horn",
                  clustering = "ward.D2")

#Bombetoka biodiversity
clustering_Ap(data.ab = bom.ab,normalization = "hellinger",
                  dissimilarity = "horn",
                clustering = "ward.D2")

################### Analyse structurale#####################
#Calcul des surfaces terrieres et biovolumes
# faut se decider clairement la manipulation adoptéé pour cette donée, ON DOIT INTERPOLER LES HFUTS
# ou PAS
# reponse, beaucoup d'essences sont encores des dotylédon?????
data = data |> filter(dhp>=10) |> mutate(st = (3.14/4)*dhp/100,
              bv = 0.51 * hfut * st) |> select(-X)

#Bon on atatche notre donne pour l'analyse en global des traitements necessaires

######################Groupement en calculant les biovolumes et surface terrieres

data |> group_by(ap,site,famille) |> 
  summarise(across(.cols=c(st,bv),.fns = list(mean = ~ mean(x = .x,na.rm = TRUE),
                                              sum = ~ sum(x=.x,na.rm=TRUE)) ) )


################### INDICE DE VALEUR D'IMPORTANCE FAMILLE /GENRE##################
#L’Indice de Valeur d’Importance des espèces (IVI) caractérise la place 
#qu’occupe, au sein d’une végétation, chaque espèce par rapport à l’ensemble 
#des autres espèces. Cet indice a été mis au point par Cottam et Curtis (1956) 
#et est fréquemment utilisé pour évaluer la prépondérance spécifique en forêts 
#"tropicales (Mori et al., 1983 ; Latour, 1994 ; Corthay, 1996 ; Kouamé, 1998 ; 
#Bakayoko, 1999 ; Adou et al., 2005 ; Missa et al., 2015)


# IVI=FREQesp+ DENSesp+ DOMesp (bvaleur variaée entre 0-300)
#frequence relative del'espece(pa)/ densité relative(st)/domiance relative(st)
data
