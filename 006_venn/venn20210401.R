library(extrafont)
library(tidyverse)
setwd("F:/GEO20210303")
source("F:/GSEAdata/GO.venn.2.R")
data1 <- read.csv("./DEG-GSE138198/ht-ptc_vs_ht.csv")
data2 <- read.csv("./DEG-GSE138198/ptc_vs_ht.csv")
data3 <- read.csv("./DEG-GSE138198/ptc_vs_ht-ptc.csv")

data <- list(data1,data2,data3)
#
for (i in 1:length(data)) {
  for (j in 1:nrow(data[[i]])) {
    #条件判断是否有na，有直接跳过执行下一行
    if(is.na( unlist(strsplit(data[[i]]$gene_assignment[j]," // "))[2])) {next;}
    data[[i]]$gene_assignment[j] <- unlist(strsplit(data[[i]]$gene_assignment[j]," // "))[2]
  }
  #
  data[[i]] <- data[[i]] %>% filter(gene_assignment != "---") %>% select(gene_assignment,logFC) %>% distinct(gene_assignment,.keep_all = TRUE)
  
}

data1 <- data[[1]] 
data2 <- data[[2]]
data3 <- data[[3]]


label<-c("HT-PTC_vs_HT","PTC_vs_HT","PTC_vs_HT-PTC")
flabel<-"All"
library(openxlsx)
library(RColorBrewer) 

col=brewer.pal(9,"Paired")[c(2,4,9)]
Cairo::CairoPDF(file = str_c(flabel,".venn.statu.pdf"),family="Arial",width=6,height=4)
b <- GOVenn(data1,data2,data3,label =label, circle.col=col,plot = FALSE)
b$plot
dev.off()

names(b$table) <-gsub("A",label[1],names(b$table))
names(b$table) <-gsub("B",label[2],names(b$table))
names(b$table) <-gsub("C",label[3],names(b$table))  ###如果是两组需要去掉

for (k in 1:3){
  da <- b$table[[k]]
  da$GeneID <- row.names(da)
  genelist <- read_tsv("F:/基因名称和ID匹配表/mmu.id.alt.unique.symbol.txt") %>% 
            dplyr::select(GeneSymbol,GeneID) %>% distinct(GeneID,.keep_all = TRUE)
  da <- merge(da,genelist,by = "GeneID") 
  b$table[[k]] <- da
}



write.xlsx(x = b$table, file = paste0(flabel, ".venn.table.xlsx"),row.names = FALSE)





















sets_1 <- names(b$table)

for (i in 1:length(sets_1)) {
  
  group <- sets_1[i]
  cres <- data.frame(b$table[[i]], group)
  #cres$Entrez_id <- row.names(cres)
  #cres <- merge(cres, ann, by.x = "Entrez_id", by.y = "Entrez_ID")
  openxlsx::write.xlsx(x = cres, file = paste0(flabel, ".venn.table.xlsx"), row.names = TRUE, append = TRUE)
}


