##dendrogram
##2022-09-15
##by yanlin liao



library(ggplot2)
library('dendextend')
library(ggpubr)


exp <-     #为基因的表达量表格（来自于芯片或者二代测序），如果是RPKM/RPKM值，数值需要事先经过log2转换。
  
group <-   #导入分组信息
Groups <-  
sample <- 
  
exp_scaled <- scale(t(exp), center = TRUE, scale = TRUE)
d <- dist(exp_scaled)
hc3 <- hclust(d,method="ward.D")   #聚类方法
dend <- as.dendrogram(hc3)
dend2 <- color_labels(dend, k = 2)  
col = get_palette("lancet",length(unique(Groups)))
names(col) <- unique(Groups)
labels_colors(dend2) <- col[Groups[match(labels(dend2),sample)]]



par(mar=c(3,3,3,6),font=2,cex=1)
p1 <- plot(dend2, main =paste0("Sample Cluster (ward.D)"),horiz = TRUE)
print(p1)







