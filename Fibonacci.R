library(ggplot2)
library(viridisLite)


#FUNCTIONS
nbChiffres <- function(nb){
  i=1
  while(nb >= 10){
    nb = nb/10
    i = i+1
  }
  return(i)
}

fibo<-function(entier){
  #On part du principe qu'on rentre un entier superieur à 8.
  
  #On initialise n0 et n1 à l'itération 8 tels que n0 représente n_n-1 et n1 n_n et que n0+n1=n_n+1
  n0=5
  n1=8
  
  #C'est notre compteur d'itération
  count=8 
  #Initialisation des vecteurs qui comptent les chiffres significatifs
  premierchiffre=c(2,1,1,0,1,0,0,1,0)
  deuxiemechiffre=c(0,0,0,0,0,0,0,0,0,0)
  #Boucle qui fait les itérations
  while(count<entier+1) {
    #calcul de l'element suivant
    N=n0+n1
    #chiffre_voulus est un nombre à 2 chiffres qui est composé des 2 premiers chiffres significatifs
    chiffres_voulus=floor(N/10**(nbChiffres(N)-2))
    #On actualise les vecteurs qui comptent le nombre de chaque chiffre significatifs
    deuxiemechiffre[chiffres_voulus%%10+1]=deuxiemechiffre[chiffres_voulus%%10+1]+1
    premierchiffre[floor(chiffres_voulus/10)]=premierchiffre[floor(chiffres_voulus/10)]+1
    #On actualise nos valeurs
    count=count+1
    n0=n1
    n1=N
  }
  
  #On calcule nos probabilités théoriques
  d=1:9
  benf=log10(1+1/d)
  
  #On divise nos compteurs pour avoir les fréquences calculées
  premierchiffre=premierchiffre/entier
  deuxiemechiffre=deuxiemechiffre/entier
  
  #On fait un simple plot en superposant les fréquences calculées des 2 premiers chiffres significatifs et théoriques du premier chiffre.
  plot(1:9,premierchiffre,col="blue",pch=18,main="Analyse Fibonacci et loi de Benford",ylab="Fréquences observées",xlab="Chiffres significatifs")
  points(0:9,deuxiemechiffre,col="green",pch=18)
  points(1:9,benf,col="red",pch=15)
  segments(1:8,premierchiffre[1:8],2:9,premierchiffre[2:9],col="blue")
  segments(0:8,deuxiemechiffre[1:9],1:9,deuxiemechiffre[2:10],col="green")
  segments(1:8,benf[1:8],2:9,benf[2:9],col="red")
  legend("topright", legend=c("1er chiffre","2eme chiffre","Théorique"),col=c("blue","green", "red"), lty=1, cex=0.8)
  
  #data.frame utilisé pour un ggplot
  resultat=data.frame(values=d,theo=benf,prem=premierchiffre)
  
  #Simulation des échantillons du premier chiffre significatif
  valeurs=rep(d,5000)
  if(entier<200){
    premchiffre=replicate(5000,table(rBenf(200))/200)
  }else{
    premchiffre=replicate(5000,table(rBenf(entier))/entier)
  }
  #Data frame des simulations du premier chiffre
  donneespremier=data.frame(valeurs,premchiffre)
  
  proba = c(0.12,0.114,0.109,0.104,0.10,0.097,0.093,0.09,0.088,0.085)
  #Data frame comprenant les indices ainsi que leur probabilité théorique
  data_theorique <- data.frame(indice=c(0:9),proba)
  #Simulation d'un échantillon d'indices
  simulation_indice=rep(0:9,500)
  #Simulation d'un échantillon de probabilité
  if(entier<200){
    simulation_proba=replicate(500,table(sample(x = 0:9,size = 200,replace = TRUE,proba))/200)
  }else{
    simulation_proba=replicate(500,table(sample(x = 0:9,size = entier,replace = TRUE,proba))/entier)
  }
  #Data frame comprenant les simulations du deuxieme chiffre
  data_simulation=data.frame(simulation_indice,simulation_proba)
  
  resultat2=data.frame(values=0:9,theo=proba,prem=deuxiemechiffre)
  
  #ggplot pour le premier chiffre significatif
  p <- ggplot(NULL,aes(x,y))+
       geom_violin(data=donneespremier,aes(factor(valeurs), premchiffre,fill=factor(valeurs)),trim = F)+
       scale_fill_viridis(discrete=TRUE,begin=0,end=1,alpha=.8,option = "plasma")+
       geom_point(data=resultat,aes(factor(values),prem,fill=factor(values)),colour="red")+
       geom_line(data=resultat,aes(x=values,y=prem),colour="red")+
       geom_point(data=resultat,aes(factor(values),theo,fill=factor(values)))+
       geom_line(data=resultat,aes(x=values,y=theo))+
       guides(fill=guide_legend("Chiffres"))+
       labs(title="Analyse Fibo & Benford")+
       xlab("Premier chiffres significatifs")+
       ylab("Fréquences observées")+
       theme(plot.title=element_text(hjust=0.5,size=25,face="bold"))+
       theme(plot.subtitle = element_text(size=10))+
       theme(axis.title.x = element_text(hjust=0.5,size=15))+
       theme(axis.title.y=element_text(hjust=0.5,size=15))+
       theme(legend.title = element_text(size=12))

  #ggplot pour le deuxieme chiffre significatif
  pp <- ggplot(NULL,aes(x,y))+
        geom_violin(data=data_simulation,aes(factor(simulation_indice), simulation_proba,fill=factor(simulation_indice)),trim = F)+
        scale_fill_viridis(discrete=TRUE,begin=0,end=1,alpha=.8,option = "plasma")+
        geom_point(data=resultat2,aes(factor(values),prem,fill=factor(values)),colour="red")+
        geom_line(data=resultat2,aes(x=values+1,y=prem),colour="red")+
        geom_point(data=resultat2,aes(factor(values),theo,fill=factor(values)))+
        geom_line(data=resultat2,aes(x=values+1,y=theo))+
        guides(fill=guide_legend("Légende"))+
        labs(title="Graphique de la répartition du deuxieme chiffre",subtitle="Projet L3MI")+
        xlab("Valeurs des chiffres")+
        ylab("Proportion des chiffres")+
        theme(plot.title=element_text(hjust=0.5,size=25,face="bold"))+
        theme(plot.subtitle = element_text(size=10))+
        theme(axis.title.x = element_text(hjust=0.5,size=15))+
        theme(axis.title.y=element_text(hjust=0.5,size=15))+
        theme(legend.title = element_text(size=12))
  
  return(list(p=p,pp=pp))
}
