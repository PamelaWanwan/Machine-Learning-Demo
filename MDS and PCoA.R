# MDS and PCoA in R

library(ggplot2)

# read in data
load(file = "03_data_out/GSE192560_sub.mrna.exp_symbols.rda")
head(sub.exp)
data.matrix <- as.matrix(sub.exp)
rm(sub.exp)

# step 1, creat a distance matrix
distance.matrix <- dist(scale(t(data.matrix), center = TRUE, scale = TRUE),
                        method = "euclidean")
# step 2, perform multi-dimensional scaling on the distance matrix
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE)

# eig = TRUE return the eigen values
# use to calculate how much variation in the distance matrix 
# each axis in the final MDS plot accounts for

# x.ret = TRUE return both rows and columns are centered version of the distance matrix

mds.stuff.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.stuff.per

mds.values <- mds.stuff$points

mds.data <- data.frame(sample=rownames(mds.values),
                      X=mds.values[,1],
                      Y=mds.values[,2])
mds.data$sample<- c(paste0(rep("PTC_HT", 5), 1:5), 
                    paste0(rep("PTC",5),1:5))
mds.data

ggplot(data=mds.data, aes(x=X, y=Y, label= sample)) + 
  geom_text() + 
  xlab(paste0("MDS1 - ", mds.stuff.per[1], "%", sep = "")) +
  ylab(paste0("MDS2 - ", mds.stuff.per[2], "%", sep = "")) +
  theme_bw() + 
  ggtitle("MDS plot using Euclidean distance")
# this is exactly the same as PCA

# use the average of the absolute value of the log fold change to calculate the distance
log2.data.matrix <- log2(data.matrix)

log2.distance.matrix <- matrix(0,
                               nrow=ncol(log2.data.matrix),
                               ncol=ncol(log2.data.matrix),
                               dimnames=list(colnames(log2.data.matrix),
                                             colnames(log2.data.matrix)))

# fill the empty matrix with log fold changes
for(i in 1:ncol(log2.distance.matrix)) {
  for(j in 1:i) {
    log2.distance.matrix[i, j] <-
      mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]))
  }
}
log2.distance.matrix

# perform multi-dimensional scaling on our new distance matrix
mds.stuff <- cmdscale(as.dist(log2.distance.matrix), eig = TRUE, x.ret = TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2])
mds.data
mds.data$Sample<- c(paste0(rep("PTC_HT", 5), 1:5), 
                    paste0(rep("PTC",5),1:5))
mds.data 

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using avg(logFC) as the distance")
