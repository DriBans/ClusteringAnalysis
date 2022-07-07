library(haven)    
library(dplyr)     #for data cleaning
library(cluster)   #for gower similarity and oam
library(psych)
library(corrplot)
library(ggplot2)   #for visualization
library(pvclust)
library(mclust)
library(fpc)
getwd()

#setting the working directory for the path
setwd("C:/Bansal Data/Drishti Data/Classes/DATA 5070/Assignment 4")

#loading the dataset in sav file with the help of haven package
data <- read_sav("C:/Bansal Data/Drishti Data/UDM/Winter 2022/DATA 5070/Asignacions/Asignacion4/wellbeingdataassignment4(1).sav")
head(data)

#ensuring the reproducibility for sampling
set.seed(20)
summary(data)
str(data)

data_esi <- data[6:10]
head(data_esi)
dim(data_esi)
summary(data_esi)

data_life <- data[20:27]
head(data_life)
dim(data_life)
summary(data_life)

data_psy <- data[12:17]
head(data_psy)
dim(data_psy)
summary(data_psy)

data_spirit <- data[30:32]
head(data_spirit)
dim(data_spirit)
summary(data_spirit)

data_pier <- data[33:38]
head(data_pier)
dim(data_pier)
summary(data_pier)

data_without_religaff <- data$religaff <- NULL
dim(data_without_religaff)
head(data)

fa <- factanal(data, factors = 2)

#checking if there is a missing value in the numerical points
sum(is.na(data))

data$ethnic[is.na(data$ethnic)] <- mean(data$ethnic, na.rm = TRUE)
data$marital[is.na(data$marital)] <- mean(data$marital, na.rm = TRUE)
###agg <- aggregate(data$marital, by = list(data3$religaff), FUN = length)
###agg

#strings will be converted to factors when using the data.frame()
religaff <- data$religaff %>%
 factor(levels = c("Catholic", "Lutheran", "African Methodist Episcopal",
                   "AME", "American Baptist", "Apostolic", "Atheist",
                   "Apostolic/Pentecostal", "Baptist", "Catholic-Not 
                   practicing", "Chaldean Catholic", "Christian", "Christian (C.O.G.I.R)",
                   "Christian/Church of the Nazare", "Church of God in Christ",
                   "Congregational", "Druze (sect of Islam", "Eastern Orthodox", "Episcopalian",
                   "HAve a God concept", "Hinduism", "Independent Gnostic", "Islam", "Jewish",
                   "Jewish,Christian", "Ken Sai", "Methodist", "Muslim", "New Thought", "None",
                   "Non-Denominational", "Pentecostal", "Presbyterian", "Primitive Baptist",
                   "Protestant", "Roman Catholic", "Russian Orthodox ", "Saved, Satisfied, etc..."),
        ordered = TRUE)
religaff


#Scaling for standardizing the variables
scaleddata <- scale(data)
head(scaleddata)
round(scaleddata, 2)
sum(is.na(scaleddata))
dim(scaleddata)

summary(scaleddata)
gower_mat <- as.matrix(scaleddata)
round(gower_mat, 2)

#Dissimilarity Matrix
d <- dist(scaleddata, method = "euclidean")

#partitioning k means-Determine number of clusters
was <- (nrow(scaleddata) -1) * sum(apply(scaleddata, 2, var))
for (i in 2:15) was[i] <- sum(kmeans(scaleddata, centers = i)$withins)

plot(1:15, was, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")

#kmeans cluster analysis
fit <- kmeans(scaleddata, 2)   #2 is the solution
#get cluster means
aggregate(scaleddata, by = list(fit$cluster), FUN = mean)
#append cluster assignment
scaleddata <- data.frame(scaleddata)
head(scaleddata)

data1 <- data[4:38]
head(data1)

#Ward hierarchical clustering
df <- dist(data1, method = "euclidean")
hclus <- hclust(df, method = "ward.D")
plot(hclus, cex = 0.6, xlab = "Hierarchical clustering using Ward")

#ward hierarchical clustering with Bootstrapped p values
fit <- pvclust(data1, method.hclust = "ward", method.dist = "euclidean")
plot(fit)
pvrect(fit, alpha = 0.95)

# Compute with agnes to get agglomerative coefficient
hc2 <- agnes(data1, method = "complete")
hc2$ac

# methods to assess
m <- c( "Average", "Single", "Complete", "Ward")
names(m) <- c( "Average", "Single", "Complete", "Ward")

# function to compute coefficient
ac <- function(x) {
  agnes(scaleddata, method = x)$ac
}

map_dbl(m, ac)


hc3 <- agnes(scaleddata, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

#ward hierarchical clustering with Bootstrapped p values
fit <- pvclust(scaleddata, method.hclust = "ward", method.dist = "euclidean")
plot(fit)
pvrect(fit, alpha = 0.95)

# Cut tree into 2 groups
hclus1 <- hclust(df, method = "ward.D2")
sub_grp <- cutree(hclus1, k = 2)

# Number of members in each cluster
table(sub_grp)

data %>%
  mutate(cluster = sub_grp) %>%
  head

plot(hclus1, cex = 0.6)
rect.hclust(hclus1, k = 4, border = 2:5)

fviz_cluster(data = scaleddata, cluster = sub_grp)

fviz_nbclust(scaleddata, FUN = hcut, method = "silhouette")

#model based
fit <- Mclust(scaleddata)
plot(fit)
summary(fit)

0#correlation
datamatrix <- cor(data[6:10])
datamatrix
corrplot(datamatrix, method = 'ellipse', hclust.method = "complete")

k3 <- kmeans(scaleddata, centers = 3, nstart = 25)
k4 <- kmeans(scaleddata, centers = 4, nstart = 25)
k5 <- kmeans(scaleddata, centers = 5, nstart = 25)

#kmeans cluster analysis
fit <- kmeans(scaleddata, 2)   #2 is the solution

fviz_cluster(k3, data = scaleddata)

# plots to compare
p1 <- fviz_cluster(fit, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#vary parameters for most readable graph
clusplot(scaleddata, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
plotcluster(scaleddata, fit$cluster)

#
fit <- kmeans(scaleddata, 2)
library(fpc)
plotcluster(scaleddata,fit$cluster, pointsbyclvecd = FALSE)
