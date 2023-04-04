##PCA
##2022-09-15
##by yanlin liao



library('FactoMineR')
library(factoextra)
library(grid)
library(ggpubr)


exp <-  read.table("D:/分析项目/在线交互画图/analysisshow-ylliao/PCA.txt")   #基因的表达量表格（来自于芯片或者二代测序），如果是RPKM/RPKM值，数值需要事先经过log2转换。
  
Group <-  read.table("D:/分析项目/在线交互画图/analysisshow-ylliao/PCAgroup.txt",header = TRUE) #样本分组信息

Groups <- Group$group
  
fontfam="Times"
pca <- prcomp(t(exp), retx=T, scale.=T)              #行为基因，列为样本，数据经过转置和归一化处理
p <- fviz_pca_ind(pca,
                   geom.ind = "point", # show points only (but not "text")
                   col.ind = Groups, # color by groups
                   palette = "lancet",
                   addEllipses = TRUE, # Concentration ellipses
                   legend.title = "group",
                   ggtheme = theme_minimal(base_size=18,base_family =fontfam),title="PCA")






