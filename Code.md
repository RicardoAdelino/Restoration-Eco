Some packages should be installed directly from the github repository. 
The following code allows to install the factoextra package using github repository as source.

```
if(!require(devtools)) install.packages("devtools")
devtools::install_url("https://github.com/wilkelab/cowplot/archive/0.6.3.zip")
devtools::install_github("kassambara/factoextra") 
```

## Loading packages
```
library(factoextra)
library(tidyr)
library(cluster)
library(randomcoloR)
library(ggrepel)
library(betapart)
library(vegan)
```

## Bird’s functional traits
``` 
tabela<-read.csv("traits_per.csv", header = T, dec = ",")  
colnames(tabela)<-c("specie", "Id","Diet.Inv", "Diet.vert", "Diet.Fruit","Diet.Nect", "Diet.Seed","Diet.PlantO","Diet.Cat","ForStrat.ground","ForStrat.understory","ForStrat.midhigh","ForStrat.canopy","ForStrat.aerial","Strat.CAT","BodyMass.Value","Size.CAT") 
att.data<-tabela[,-c(1,2,9,15,17)]/100 
dados<-(t(att.data)) 
colnames(dados)<-tabela$specie
dados.scl<-scale(dados)
dados.redim<-t(dados.scl)
```
## Hybrid K means with K-number of clusters.
This function calculates the Hybrid K-means from 2 to 30 groups.

```
k.max <- 30 
sil <- rep(0, k.max) 
for(i in 2:k.max){
  hkm.res <- factoextra::hkmeans(dados.redim, k = i, iter.max = 100)
  ss <- cluster::silhouette(hkm.res$cluster, dist(dados.redim))
  sil[i] <- mean(ss[, 3])
}
```

## Detect the the optimal number of K-grous for the data 
This step allows to generate the *Supplementary figure 3D*
```
tst <- data.frame(sil = sil, k = seq(1, k.max, 1))
plot_sil<-ggplot(data=tst[-1,], aes(x=as.factor(k), y=as.factor(round(sil,3)), group=1)) +  geom_line() +  geom_point(data=tst[c(5,8,15),], colour="red", aes(x=as.factor(k), y=as.factor(round(sil,3))), size=2) + geom_text_repel(data=tst[c(5,8,15),], aes(label=c("I","II","III"))) + labs(x = "Number of groups",y = "Mean Silhouette") + theme(legend.position = "none") + theme_classic() 
```

## Hybrid K-means with optimal number of K-groups (N=15). 
Here we are indicating only the code to generate for **K = 15** groups. To generate the *Table 3* in main text, it is necessary execute this code replacing **K =15** for **K=5** and **K=8**. 
```
hkm.res <- hkmeans(dados.redim, k = 15, iter.max = 100) 
sil = silhouette (hkm.res$cluster, dist(dados.redim))
```

## Traits that significantly contributed to the formation of the cluster 
For **K = 15** see *Supplementary table 3* 
```
v.test <- data.frame(cbind(cluster = hkm.res$cluster, dados.redim))
v.test$cluster <- as.factor(v.test$cluster)
ct.res <- FactoMineR::catdes(v.test, proba = 0.05, num.var = 1)
```

## Retaining information about the silhouette
This step allows identifying which group each species fall within. In this step, the group is represented by factor *i.e. 1,2,3...*. For more details on how to create the functional groups' names see *Labelling the functional groups* section in the main text. Here we are indicating only the code to clustering species for **K = 15** groups. In order to get a more comprehensive effect of the number of groups in species clustering it will be necessary to execute this code replacing **K =15** for **K=5** and **K=8**.

```
sil.info<-as.data.frame(sil[,1:3])
sil.sp<-data.frame(sil.info,tabela$specie)
sil.data<-sil.sp[order(sil.sp$cluster, -sil.sp$sil_width), ]
```


## Restoration age 
This step allows to generate the *Supplementary figure 5*

```
lista<-read.csv('Tabela por papers2.csv', header = T)
clust_list.bin <- lapply(groups.clust, function(x){data.frame(lista[lista$X %in% row.names(x),])})
clust_list.bin2<-lapply(clust_list.bin,"[", -1)
repr<-lapply(clust_list.bin2, function(x){apply(x, 2, sum)})
rep.table<-data.frame(do.call(cbind,repr))
overall.group<-colSums(rep.table)
rep.table<-rbind(rep.table,overall.group)
row.names(rep.table)<-c(row.names(rep.table[-15,]),"overall")
rep.table$Paper<-rownames(rep.table)
rep.table<-data.frame(do.call(cbind,repr))[-1,]
rep.table$ages<-factor(c('Recent','Recent','Recent','Intermediate','Intermediate','Intermediate','Intermediate','Intermediate','Old','Old','Old','Old','Old'),levels = c("Recent", "Intermediate", "Old"))
tidy.raw<-rep.table %>% gather(rep.table, Riqueza, Group.1:Group.15)
#tidy.raw$Paper<-factor(tidy.raw$Paper)
tidy.raw$rep.table<-factor(tidy.raw$rep.table, levels = c("Group.1","Group.2","Group.3","Group.4","Group.5",  "Group.6",  "Group.7",  "Group.8",  "Group.9","Group.10","Group.11", "Group.12", "Group.13", "Group.14", "Group.15"))

levels(tidy.raw$rep.table)<-c("Middle insectivores of inferior stratum","Small nectarivores of inferior stratum","Small omnivores of inferior stratum","Small granivores of inferior stratum","Small omnivores of all strata",  "Middle frugivores and insectivores of inferior stratum",  "Large omnivores of all strata",  "Small insectivores of inferior stratum",  "Large frugivores of all strata","Middle frugivores and insectivores of middle stratum","Large insectivores of middle stratum", "Small insectivores of middle stratum", "Middle omnivores of inferior stratum ", "Small omnivores of superior stratum", "Small frugivores of superior stratum")

boxp<-ggplot(tidy.raw, aes(x=ages, y=Riqueza, fill=ages)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Dark2") +  
  facet_wrap(~rep.table, ncol=3) +
  labs(fill = "Groups") + 
  xlab("Restoration ages") +
  theme_bw() +
  theme(strip.text.x = element_text(face="bold"), legend.position="none", strip.background = element_rect(colour = 'black', fill = 'white')) 
```

## Bird assemblage and restoration ages 
This step allows to generate the *main table 2* and the *ordination plot*.

```
lista<-read.csv('Tabela por papers2.csv', header = T)
lista2<-t(lista[,-c(1,2)])
groups <- factor(c(rep(1,3), rep(2,5), rep(3,5)), labels = c("new","int","old"))
core<-betapart.core(lista2)
beta.multi(lista2,index.family="jaccard")
dist<-beta.pair(lista2, index.family="jaccard")
matrix(c(mean(dist$beta.jtu),mean(dist$beta.jne),sd(dist$beta.jtu),sd(dist$beta.jne)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Mean", "SD"),c("Turnover", "Nestedeness")))
bd<-betadisper(dist[[3]],groups,sqrt.dist=T)
plot(bd,ellipse = TRUE, hull = FALSE,ylim=c(-.5,.5))
set.seed(1234)
permutest(bd, pairwise = TRUE, permutations = 999)
TukeyHSD(bd)
```

