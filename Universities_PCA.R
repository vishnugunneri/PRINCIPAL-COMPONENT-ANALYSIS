install.packages("kableExtra")
install.packages("dplyr")
#load data
universities.df <- read.csv("D:/My_Projects_R/DMBA-R/DMBA-R-datasets/DMBA-R-datasets/Universities.csv")


pcs <- prcomp(na.omit(universities.df[,-c(1:3)]))
summary(pcs)

#PCA OUTPUT USING ALL NORMALIZED 17 NUMERICAL VARIABLES IN THE
#Universities DATASET. THE TABLE SHOWS RESULTS FOR THE FIRST FIVE
#PRINCIPAL COMPONENTS

pcs.cor <- prcomp(na.omit(universities.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)

pcs.cor$rotation[,1:7]

#Get Eigen Values of Principal Components
#Adopt the one with eigenvalue greater than 1
eig <- (pcs.cor$sdev)^2
eig

#Get Variance of principal components
variance <- eig*100/sum(eig)
variance

#Cumulative Variances
cumvar <- cumsum(variance)
cumvar

#combine to dataframe
eig.var.cum <- data.frame(eig = eig,variance = variance , cumvariance = cumvar)
head(eig.var.cum)

#barplot of pc eigenvalues
barplot(eig.var.cum[1:6,1] , names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6"))

#barplot of pc variances
barplot(eig.var.cum[1:6,2] , names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6"))

#Screeplot
screeplot(pcs.cor, type = "lines", main = "Scree Plot - University Data")
box()
abline(h = 1, lty = 2)

pca_loadings <- pcs.cor$rotation
kable(pca_loadings, caption = "PCA Loadings") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


