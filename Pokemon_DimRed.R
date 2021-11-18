#Dimensionality reduction of Pokémon Data!
#The dataset may be downloaded here: https://www.kaggle.com/rounakbanik/pokemon

# Import section ----------------------------------------------------------


library(ggplot2)
library(caret)
library(lle)
library(MASS)
library(Rtsne)
library(umap)
library(gridExtra)


# Data preparation --------------------------------------------------------
df_poke<-read.csv('pokemon.csv')
summary(df_poke)

df_poke$height_m[df_poke$name=='Rattata']<-0.3
df_poke$height_m[df_poke$name=='Raticate']<-0.7
df_poke$height_m[df_poke$name=='Raichu']<-0.7
df_poke$height_m[df_poke$name=='Sandshrew']<-0.7
df_poke$height_m[df_poke$name=='Sandslash']<-1
df_poke$height_m[df_poke$name=='Vulpix']<-0.6
df_poke$height_m[df_poke$name=='Ninetales']<-1.1
df_poke$height_m[df_poke$name=='Diglett']<-0.2
df_poke$height_m[df_poke$name=='Dugtrio']<-0.7
df_poke$height_m[df_poke$name=='Meowth']<-0.4
df_poke$height_m[df_poke$name=='Persian']<-1.0
df_poke$height_m[df_poke$name=='Geodude']<-0.4
df_poke$height_m[df_poke$name=='Graveler']<-1.0
df_poke$height_m[df_poke$name=='Golem']<-1.4
df_poke$height_m[df_poke$name=='Grimer']<-0.9
df_poke$height_m[df_poke$name=='Muk']<-1.2
df_poke$height_m[df_poke$name=='Exeggutor']<-2.0
df_poke$height_m[df_poke$name=='Marowak']<-1.0
df_poke$height_m[df_poke$name=='Hoopa']<-0.5
df_poke$height_m[df_poke$name=='Lycanroc']<-0.8

df_poke$weight_kg[df_poke$name=='Rattata']<-3.5
df_poke$weight_kg[df_poke$name=='Raticate']<-18.5
df_poke$weight_kg[df_poke$name=='Raichu']<-30
df_poke$weight_kg[df_poke$name=='Sandshrew']<-12
df_poke$weight_kg[df_poke$name=='Sandslash']<-29.5
df_poke$weight_kg[df_poke$name=='Vulpix']<-9.9
df_poke$weight_kg[df_poke$name=='Ninetales']<-19.9
df_poke$weight_kg[df_poke$name=='Diglett']<-0.8
df_poke$weight_kg[df_poke$name=='Dugtrio']<-33.3
df_poke$weight_kg[df_poke$name=='Meowth']<-4.2
df_poke$weight_kg[df_poke$name=='Persian']<-32.0
df_poke$weight_kg[df_poke$name=='Geodude']<-20.0
df_poke$weight_kg[df_poke$name=='Graveler']<-105.0
df_poke$weight_kg[df_poke$name=='Golem']<-300.0
df_poke$weight_kg[df_poke$name=='Grimer']<-30.0
df_poke$weight_kg[df_poke$name=='Muk']<-30.0
df_poke$weight_kg[df_poke$name=='Exeggutor']<-120.0
df_poke$weight_kg[df_poke$name=='Marowak']<-45.0
df_poke$weight_kg[df_poke$name=='Hoopa']<-9.0
df_poke$weight_kg[df_poke$name=='Lycanroc']<-25.0

df_poke$capture_rate<-as.numeric(df_poke$capture_rate)
df_poke$capture_rate[is.na(df_poke$capture_rate)]<-30
df_poke$percentage_male[is.na(df_poke$percentage_male)]<-0

rank <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,2,1,2,2,3,1,2,1,2,3,1,2,3,2,3,1,
          2,2,3,1,2,1,2,3,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,
          2,3,1,2,1,2,1,2,1,1,2,1,2,1,2,1,2,1,2,3,1,1,2,1,2,1,2,1,2,1,2,2,2,1,1,2,1,
          2,2,1,1,1,2,1,2,1,2,2,1,2,2,2,1,1,1,2,1,1,1,2,2,2,1,1,2,1,2,1,2,1,1,1,1,2,
          3,1,1,1,2,3,1,2,3,1,2,3,1,2,1,2,1,2,1,2,3,1,2,1,1,1,1,2,1,2,1,2,3,3,2,3,2,
          3,1,2,3,1,1,2,1,1,2,2,2,1,2,1,1,2,1,1,2,1,1,2,1,2,1,2,1,1,1,1,2,1,2,1,2,1,
          1,2,1,2,1,1,2,3,1,2,2,1,1,1,2,1,1,1,1,2,1,1,1,1,2,3,1,1,1,1,2,3,1,2,3,1,2,
          3,1,2,1,2,1,2,3,2,3,1,2,3,1,2,3,1,2,1,2,1,2,3,1,2,1,2,1,2,3,1,2,2,1,2,3,1,
          2,3,1,1,2,1,1,1,2,3,1,2,1,2,1,1,1,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,3,1,2,1,
          2,1,1,1,1,1,2,1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,1,1,2,1,2,3,1,2,2,1,1,
          1,2,3,1,2,3,1,1,1,1,1,1,1,1,1,1,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,2,1,2,3,1,3,
          1,2,1,2,1,2,2,1,2,1,1,2,1,2,1,2,2,1,2,1,2,2,2,1,2,1,1,2,1,2,1,1,1,1,1,1,2,
          3,1,1,2,1,2,1,2,1,2,1,1,2,1,1,2,2,3,2,3,2,3,3,3,2,2,2,2,3,3,3,2,3,2,1,1,1,
          1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,2,3,1,2,3,1,2,1,2,3,1,2,1,2,1,2,1,2,1,2,
          1,2,3,1,2,1,2,3,1,2,1,2,1,1,2,3,1,2,3,1,1,1,2,3,1,2,3,1,2,1,2,1,1,2,3,1,2,
          1,1,2,1,2,1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,3,1,2,3,1,2,1,2,3,1,2,1,1,2,1,2,1,
          2,1,1,2,1,2,1,2,3,1,2,3,1,2,1,2,3,1,2,3,1,2,1,1,2,1,1,2,1,1,2,1,2,1,1,2,1,
          2,1,1,1,2,3,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,2,3,1,2,3,1,2,1,2,3,1,2,3,
          1,2,1,2,3,1,2,1,2,1,1,2,1,2,3,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,2,1,1,1,
          1,2,3,1,1,2,1,2,1,2,1,2,1,1,1,1,1,1,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,2,3,1,2,
          1,1,2,1,2,1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,3,1,1,1,1,2,1,2,1,1,1,1,1,1,1,
          1,1,1,1,1,2,3,1,1,1,1,1,2,3,3,1,1,1,1,1,1,1,1,1)
#Changing legendary pokémon rank to 4
rank[df_poke$is_legendary==1]<-4
#Set rank as factor
rank<-factor(rank)
#Name the levels
levels(rank)<-c('Rank 1', 'Rank 2', 'Rank 3', 'Legendary')
#
pokemon<-data.frame(id=df_poke$pokedex_number,
                    scale(df_poke[,c('attack','defense','speed','sp_attack',
                                     'sp_defense','hp')]))


pokenames<-df_poke$name



# function for 2d map plotting --------------------------------------------
map_pokemon <- function(dim1,dim2,rank,title='MAP',selec=NULL,pokenames) {
  #create ggplot
  p<-ggplot()
  #add points mapped in two dimensions and target color
  p<-p+geom_point(aes(x=dim1,y=dim2,fill=rank),
                  shape=21,alpha=0.5,size=3,colour='black')
  #highlight selected points
  p<-p+geom_text(aes(x=dim1[selec],y=dim2[selec]+0.5),
                 label=pokenames[selec],size=4,fontface = "bold")+
    geom_point(aes(x=dim1[selec],y=dim2[selec]),
               size = 5, alpha = 1,na.rm = T, shape = 21, colour = "black")
  
  #axis labels and themes
  p<-p+ labs(title = title, x = 'Dim 1', y = "Dim 2")
  p<-p+ theme_light()+theme(plot.title=element_text(size=20,hjust=0.5),
                            axis.text=element_text(size=14,face='bold'),
                            axis.title=element_text(size=20,face="bold"),
                            legend.title=element_text(size=15),
                            legend.text=element_text(size=15),
                            legend.box.background = element_rect(colour = "black"))
  return(p)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Feature importance with random forest -----------------------------------
control<-rfeControl(functions=rfFuncs,method='cv',number=15)
results<-rfe(pokemon[,2:dim(pokemon)[2]],rank,sizes=c(1:dim(pokemon)[2]),
                rfeControl=control)
imps <- data.frame(var = row.names(varImp(results)),
                   imp = varImp(results)[, 1])

ggplot(data = imps, 
       aes(x = reorder(var, -imp), y = imp, fill = rainbow(length(var)))) +
  geom_bar(stat="identity") + labs(x = "Variable", y = "Importance") + 
  geom_text(aes(label = round(imp, 1)), vjust=2, color="white", size=6) + 
  theme_bw() + theme(text = element_text(size=18),legend.position = "none",
                     axis.text.x = element_text(angle=90,size=14))

selec<-c(1,2,3,4,5,6,7,8,9,10,25,129,147,148,149,150,151)

#select the two most important features according to random forest
pokemon.vars<-pokemon[results$optVariables[c(1,2)]]
#use them to map pokémon data
map_pokemon(pokemon.vars[,1],pokemon.vars[,2],rank,'Selected',selec,pokenames)


# Dimensionality reduction ------------------------------------------------

pokemon.pca<-prcomp(pokemon[,2:dim(pokemon)[2]],scale=TRUE)

#This line may take some time:
# calc_k(pokemon[,2:dim(pokemon)[2]], m = 2, 1, 100, plotres=TRUE,  parallel=FALSE, cpus=2, iLLE=FALSE)

pokemon.lle<-lle(pokemon[,2:dim(pokemon)[2]],m=2,k=20,ss=FALSE,id=TRUE,v=0.9)
rank2<-rank[duplicated(pokemon[,2:dim(pokemon)[2]])==FALSE]
pokemon<-pokemon[duplicated(pokemon[,2:dim(pokemon)[2]])==FALSE,]
pokemon.sammon <- sammon(dist(pokemon[,2:dim(pokemon)[2]]),k=2)
pokemon.mds<-isoMDS(dist(pokemon[,2:dim(pokemon)[2]]),k=2)
pokemon.tsne<-Rtsne(as.matrix(pokemon[,2:dim(pokemon)[2]]),perplexity=30,dims=2)
pokemon.umap<-umap(as.matrix(unique(pokemon[,2:dim(pokemon)[2]])))
pokemon.umap<-predict(pokemon.umap,pokemon[,2:dim(pokemon)[2]])



# Plots -------------------------------------------------------------------
p1<-map_pokemon(pokemon.pca$x[,1],pokemon.pca$x[,2],rank,'PCA',selec,pokenames)
p2<-map_pokemon(pokemon.lle$Y[,1],pokemon.lle$Y[,2],rank,'LLE',selec,pokenames)
p3<-map_pokemon(pokemon.sammon$points[,1],pokemon.sammon$points[,2],rank2,'SAMMON',selec,pokenames)
p4<-map_pokemon(pokemon.mds$points[,1],pokemon.mds$points[,2],rank2,'isoMDS',selec,pokenames)
p5<-map_pokemon(pokemon.tsne$Y[,1],pokemon.tsne$Y[,2],rank2,'t-SNE',selec,pokenames)
p6<-map_pokemon(pokemon.umap[,1],pokemon.umap[,2],rank2,'UMAP',selec,pokenames)

legend<-get_legend(p1)
lay<-rbind(c(1,1,1,1,1,2,2,2,2,2,99,7,7),
           c(1,1,1,1,1,2,2,2,2,2,99,7,7),
           c(1,1,1,1,1,2,2,2,2,2,99,7,7),
           c(1,1,1,1,1,2,2,2,2,2,99,7,7),
           c(3,3,3,3,3,4,4,4,4,4,99,7,7),
           c(3,3,3,3,3,4,4,4,4,4,99,7,7),
           c(3,3,3,3,3,4,4,4,4,4,99,7,7),
           c(3,3,3,3,3,4,4,4,4,4,99,7,7),
           c(5,5,5,5,5,6,6,6,6,6,99,7,7),
           c(5,5,5,5,5,6,6,6,6,6,99,7,7),
           c(5,5,5,5,5,6,6,6,6,6,99,7,7),
           c(5,5,5,5,5,6,6,6,6,6,99,7,7))
p1<-p1+theme(legend.position='none')
p2<-p2+theme(legend.position='none')
p3<-p3+theme(legend.position='none')
p4<-p4+theme(legend.position='none')
p5<-p5+theme(legend.position='none')
p6<-p6+theme(legend.position='none')

grid.arrange(p1, p2,p3, p4,p5,p6, legend, ncol=4, layout_matrix=lay)

# Selection of features including biological data -----------------------------------------------


pokemon<-data.frame(id=df_poke$pokedex_number,
                    scale(df_poke[,c('attack','defense','sp_attack',
                               'sp_defense','hp','speed','height_m','weight_kg',
                               'percentage_male','experience_growth',
                               'base_happiness','capture_rate','base_egg_steps')]))

