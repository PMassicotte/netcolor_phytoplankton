# Model qui relie le coefficient d'absorption du phytoplankton
# a la concentration en chlorophyll-a base sur l'article Devred et al. 
# (2006 JGR). 

# ici le model a ete developpe pour 7 longueurs d'onde (MERIS) mais
# il est facilement modifiable pour n'importe quelle autre longeurs d'onde

library(robustbase)
library(limSolve)

lambda = c(412,443,490,510,560,620,665) # WV MERIS

f <- function(as1,as2,S) # Model pour 3 parametres
{(as1-as2)/S*(1.-exp(-S*x))+as2*x } 

fns <- function(as1,as2) # Model pour 2 parametres ou S est fixe. 
{(as1-as2)/Sf*(1.-exp(-Sf*x))+as2*x}

# on definie les parametres astar1 et astar2 qui sont les coefficients
# d'absorption specifique ds population de phyto1 et phyto2
astar1 <- rep(0,7)
astar2 <- rep(0,7)
# Interface de confiance
cias1 = matrix(0,7,2)
cias2 = matrix(0,7,2)
# deviation standard. 
sdas1 = rep(0,7)
sdas2 = sdas1


# Ce morceau de code utilise la fonction covMcd pour 
# enlever les "outliers" 
indgood = rep(TRUE,length(chlis))
for (j in 1:7){
c1 <- covMcd(cbind(log10(chlis),log10(ap[,j])), alpha = 0.5)
mahmax <- max(c1$mah)
normah <- c1$mah/mahmax
indout <- normah > 0.5
indgood[indout] <- FALSE
}

# ici je ne garde que les "bonnes" absorptions et concentrations en chl
x <- chlis[indgood]
y <- ap[indgood,2]

# j'usilise le modele a 3 parametres pour lambda = 443 nm (peak de chl)
# pour retrouver le parametre S ()
res <- nls(y ~ f(as1,as2,S),start=list(as1=0.1,as2=0.05,S=3.))
Sf = as.numeric(coefficients(res)[3]) # S est stocke dans la variable Sf


# Maintenant, j'applique le modele a 2 parametres pour les 7 longueurs d'ondes
# S est fixe et je cherche astar1 et astar2 qui sont les coefficients d'absorption
# des populations 1 et 2. 
for (i in 1:7){
y <- ap[indgood,i]
res <- nls(y ~ fns(as1,as2),start=list(as1=0.08,as2=0.04))
astar1[i] <- coefficients(res)[1] 
astar2[i] <- coefficients(res)[2] 
cias1[i,] =   as.numeric(confint(res,level=0.99)[1,])
cias2[i,] =   as.numeric(confint(res,level=0.99)[2,])
sdas1[i] = cias1[i,2] - astar1[i]
sdas2[i] = cias2[i,2] - astar2[i]
}

# on fait une figure.
par(mfcol=c(2,1))
plot(lambda,astar1,ylim=c(0,0.12))
points(lambda,astar1+sdas1,col="red")
points(lambda,astar1-sdas1,col="red")
plot(lambda,astar2,ylim=c(0,0.04))
points(lambda,astar2+sdas2,col="red")
points(lambda,astar2-sdas2,col="red")

