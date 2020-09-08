df <- dat[,1:12]
df <- scale(df)
df <- as.data.frame(df)

#
df <- transform(df, first_rank = max(first_rank) - first_rank)
cor(df)
round(cor(df), digits = 3)
par(mar=c(1,1,1,1))
plot(df)

df <- df[,c(20:35)]
str(df)
library(nFactors)
ev <- eigen(cor(df))
ap <- parallel(subject = nrow(df), var = ncol(df), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

#
df_factanal <- factanal(df, factors = 2, rotation = "varimax",
                        scores = "regression")
print(df_factanal$loadings, cutoff = 0)



df_factanal$scores


library(psych)
KMO(df)
#값이 낮아서 세 개 지움
df2 <- df[,c(3:10, 12)]

ev <- eigen(cor(df))
ap <- parallel(subject = nrow(df), var = ncol(df), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)
df2_factanal <- factanal(df, factors = 2, rotation = "varimax",
                         scores = "regression")
print(df2_factanal$loadings, cutoff = 0)

fadf <- as.data.frame(df_factanal$scores)


##################################################################
fadf <- scale(fadf)
fadf <- as.data.frame(fadf)

###############################################hclust
x <- NbClust(fadf, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
x <- NbClust(fadf, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D")

plot(prcomp(fadf), type="l", sub = "Scree Plot")

fadf.pca <- PCA(fadf, ncp = 2, graph = FALSE)
fadf.pca$eig
fadf.hcpc <- HCPC(fadf.pca, graph = FALSE, 2)
head(fadf.hcpc$data.clust, 10)
z <- as.data.frame(fadf.hcpc$data.clust)
fviz_cluster(fadf.hcpc)
table(fadf.hcpc$data.clust$clust)
###
ggplot(aes(x = Factor1, y = Factor2), data = z) +
  geom_point(aes(color = z$clust))
###
df1 <- df
df1$cluster <- fadf.hcpc$data.clust$clust

k <- 2
results <- df1 %>%
  mutate(cluster = fadf.hcpc$data.clust$clust) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
results$the_summary




###############################################kmeans cluster
pc <- prcomp(fadf)
comp <- data.frame(pc$x)
kmeans <- kmeans(comp, 2, nstart = 25, iter.max = 1000)
head(kmeans)
library(RColorBrewer)
library(scales)
library(rgl)
table(kmeans$cluster)
help <- as.factor(kmeans$cluster)

comp$cluster <- kmeans$cluster
df2$cluster <- comp$cluster

plot3d(comp$PC1, comp$PC2, comp$PC3, col=kmeans$clust)
ggplot(as.data.frame(pc$scores), aes(x=comp$PC1, y=comp$PC2, color=help)) +
  geom_point(size=2) + theme_bw() + 
  ggtitle('First 2 prinipal components clustering w/ kmeans') + 
  xlab("Principal Component 1") + ylab("Principal Component 2")

ggplot(aes(x = comp$PC1, y=comp$PC2), data = comp) +
  geom_point(aes(color = help))

k <- 2
results <- df2 %>%
  mutate(cluster = kmeans$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
results$the_summary


###############################################kmedoidss cluster(pam)
#gower distance

gower_dist <- daisy(fadf, metric = "gower")
gower_mat <- as.matrix(gower_dist)

sil_width <- c(NA)
for (i in 2:5) {
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

par(mar=c(1,1,1,1))
plot(1:5, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:5, sil_width)

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
library(dplyr)
library(magrittr)
library(caret)


df3 <- df
df3$cluster <- pam_fit$clustering


pam_results <- df3 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
table(pam_fit$clustering)

library(ggplot2)
library(Rtsne)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

###
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
###