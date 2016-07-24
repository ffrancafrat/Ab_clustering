#! /usr/bin/R

library("ggplot2");
library("cluster");

#_______________________________________________________________________________
## input
dat.l.raw = read.table("light_interface_scores.dat", header = TRUE);
dat.h.raw = read.table("heavy_interface_scores.dat", header = TRUE);

#_______________________________________________________________________________
## prepare matrix: move PDB IDs out of the matrix and make them rownames
dat.l = dat.l.raw[ ,-1];
row.names(dat.l) = dat.l.raw[ ,1];

dat.h = dat.h.raw[ ,-1];
row.names(dat.h) = dat.h.raw[ ,1];

#_______________________________________________________________________________
## PCA
PCAggplot = function(pca.dat, pcx, pcy, ggplot.colours) {
    theme_set(theme_gray(base_size = 16));
    ggplot(data = as.data.frame(pca.dat$x), aes_string(x = pcx, y = pcy)) +
    #xlim(-700, -600) +
    #ylim(25,75) +
    geom_hline(yintercept = 0, colour = "gray65") +
    geom_vline(xintercept = 0, colour = "gray65") +
    #geom_point(alpha = 0.8, size = 2, colour = ggplot.colours) +
    geom_text(label = rownames(pca.dat$x), colour = ggplot.colours, hjust = -0.2, vjust = 0.2, size = 2.5);
}

colours = c("lightblue", "deepskyblue", "dodgerblue", "orange", "greenyellow", "orangered", "blue", "green", "red", "darkblue", "darkgreen", "darkred", "darkorange");

pca.dat.l = prcomp(dat.l, retx = TRUE, center = TRUE, scale. = FALSE);
#plot(pca.dat.l);
pl = PCAggplot(pca.dat.l, "PC1", "PC2", "black");
biplot(pca.dat.l);

pca.dat.h = prcomp(dat.h, retx = TRUE, center = TRUE, scale. = FALSE);
#plot(pca.dat.h);
ph =PCAggplot(pca.dat.h, "PC1", "PC2", "black");
biplot(pca.dat.h);

#_______________________________________________________________________________
## clustering
## coordinates of PDB IDs in PC2->PC1 space
pca.dat.l.mat = as.matrix(pca.dat.l$x);
coord12.dat.l = pca.dat.l.mat[ ,1:2];
## distance matrix with Minkowski distance
dist.dat.l = dist(coord12.dat.l, method = "minkowski", p = 4); 
## agglomerative cluster
clust.dat.l = agnes(dist.dat.l);
## large PDF plot to resolve PDB IDs
pdf(file="clusterl.pdf", height = 40, width = 80);
plot(clust.dat.l.c, which.plots = 2, cex = 1);
dev.off();

pca.dat.h.mat = as.matrix(pca.dat.h$x);
coord12.dat.h = pca.dat.h.mat[ ,1:2];
dist.dat.h = dist(coord12.dat.h, method = "minkowski", p = 4); 
clust.dat.h = agnes(dist.dat.h);
pdf(file="clusterh.pdf", height = 40, width = 80);
plot(clust.dat.h.c, which.plots = 2, cex = 1);
dev.off();

