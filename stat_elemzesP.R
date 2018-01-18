#env. paramĂ©terek -2 kvadrĂˇt, ahol nem volt Ăˇllat, -a factorok
envP <- read.csv(file="mindenesP2016.csv", sep = ";")
row.names(envP) <- envP[,1]
envP<- envP[-24,-c(1,16)]
envP_scale <- scale(envP)

## bevettem az alap env listába az RE-r, úgyh módosulhatnak a plotok, eredmények! vagy módosítani vagy kivenni!!!
##envK normalitás vizsgálata: 4 nem normál eloszlású
shapiro.test(envP[,1])
shapiro.test(envP[,2])
shapiro.test(envP[,3])
shapiro.test(envP[,4])
shapiro.test(envP[,5])
shapiro.test(envP[,6])
shapiro.test(envP[,7])
shapiro.test(envP[,8])
shapiro.test(envP[,9])
shapiro.test(envP[,10])
shapiro.test(envP[,11])
shapiro.test(envP[,12])
shapiro.test(envP[,13])
shapiro.test(envP[,14])

## fajlista relat??v abundanciával

fajlistaPetoczi <- read.csv(file="fajlista_Pet?czi.csv", sep = ";")

row.names(fajlistaPetoczi) <- fajlistaPetoczi[,1]
fajlistaPetoczi <- fajlistaPetoczi[,-1]

pca_P <- prcomp(fajlistaPetoczi)

biplot(pca_P)

print(pca_P)
summary(pca_P)
plot(pca_P, type="l")

pca_envP <- prcomp(envP_scale)
summary(pca_envP)
                    
## autopot from ggplot and ggfortify
library(ggfortify)
autoplot(pca_P)
cc_P <- strsplit(row.names(fajlistaPetoczi),"\\.") ##megkeresi az adott karaktert ?s ott 2 darabra v?gja
fajl.kvadr_P <- cbind(fajlistaPetoczi, Kvadr=unlist(cc_P)[2*(1:nrow(fajlistaPetoczi))-1])
pca_P <- prcomp(fajl.kvadr_P[-ncol(fajl.kvadr_P)])

autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'))
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), label=T)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0)
autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0, frame=T)
myplot5 <- autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0, frame=T)
myplot5 + theme_bw()
myplot6 <- autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), frame=T)
myplot6 + theme_bw()
myplot14 <- autoplot(pca_P, data=fajl.kvadr_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, frame=T)
myplot14 + theme_bw()

##PCA háttérváltozók
library(ggfortify)
autoplot(pca_envP)
cc_P2 <- strsplit(row.names(envP_scale),"\\.") ##megkeresi az adott karaktert ?s ott 2 darabra v?gja
kvadr_env_P <- cbind(as.data.frame(envP_scale), Kvadr=unlist(cc_P2)[2*(1:nrow(envP_scale))-1])
pca_P_env <- prcomp(kvadr_env_P[-ncol(kvadr_env_P)])

autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'))
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), label=T)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0)
autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0, frame=T)
myplot9 <- autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0, frame=T)
myplot9 + theme_bw()
myplot10 <- autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), frame=T)
myplot10 + theme_bw()
myplot16 <- autoplot(pca_P_env, data=kvadr_env_P, colour=as.character('Kvadr'), shape=F, label.size=2, loadings=T,loadings.label = TRUE, loadings.label.size =2, scale=0)
myplot16 + theme_bw()

##cca relatív abundanciával
fajlistaP10 <- read.csv(file="fajlista_P10.csv", sep = ";")
row.names(fajlistaP10) <- fajlistaP10[,1]
fajlistaP10 <- fajlistaP10[,-1]

envP10_scale <- envP_scale[,-c(1,4,5,8,12,13)]  ##d10,d60, Q, d_mm, k_mm nélkül

library(vegan)
cca_kulonP <- cca(fajlistaP10, envP10_scale)
sumP10cca <- summary(cca_kulonP)
plot(cca_kulonP)
plot(cca_kulonP, display = c("sp","cn")) ##mintavételi pontok nélkül
plot(cca_kulonP, display = c("wa","cn"))
plot(cca_kulonP, display = c("sp","cn"), xlim= c(-1,1), ylim=c(-2,1)) ##mintavételi pontok nélkül, belenagyítva

library(CCP) 
##https://cran.r-project.org/web/packages/CCP/CCP.pdf
##https://stackoverflow.com/questions/21672302/goodness-of-fit-in-cca-in-r

N <- dim(fajlistaP10) [1] ##megfigyelések száma
p <- dim(fajlistaP10) [2] ##függő változók száma
envP10_scale <- read.csv("envP10_scale.csv", sep=";")
q <- dim(envP10_scale) [2] ##független változók száma

cancorP <- cancor(fajlistaP10, envP10_scale, xcenter=TRUE, ycenter = FALSE)$cor
p.asym(cancorP, N, p, q, tstat = "Wilks")

CCorA(envP10_scale, fajlistaP10)
## https://stackoverflow.com/questions/5850763/canonical-correlation-analysis-in-r

## In R, the base package provides the function cancor() to enable
## CCA. This is limited to cases where the number of observations is
## greater than the number of variables (features), nrow(X) > ncol(X).

cc(fajlistaP10, envP10_scale)
cc(fajlistaP10[,-c(23,24,25,26,27,28,29,30,31)], envP10_scale)

tst <- cc(fajlistaP10[,1:10], envP10_scale)$cor
p.asym(tst, N, 10, q, tstat = "Wilks")

ttmp <- cor(fajlistaP10)
colnames(ttmp) <- NULL
rownames(ttmp) <- NULL
heatmap(ttmp,scale="none",Rowv=NA,Colv=NA)
fajlistaP10[,c(4,15)]
fajlistaP10[,c(23,29)]

N > p+q+1
correl <- matcor(fajlistaP10, envP10_scale)
img.matcor(correl, type = 2)
