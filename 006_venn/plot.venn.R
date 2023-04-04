##venn 3 种方式
##2022-09-07
##amend yanlin liao



library(extrafont)
###区分上下调 ,venn 图
##data1,data2,data3 是三个dataframe, 首列是ID,第二列是logFC,列名无所谓是什么，data3可以为空。
##method1

source("D:/代码库/006_venn/GO.venn.2.R")
label<-c("IQ.MR4D", "GM1.MR4D", "MR4D.sham") ## 设置每个数据集的名称
flabel<-"Drug" ### 保存的venn图的名称的标签


library(openxlsx)
library(RColorBrewer) 

col=brewer.pal(9,"Paired")[c(2,4,9)]

cairo_pdf(str_c(flabel,".venn.statu.pdf"),family="Arial",width=4,height=4)
b <- GOVenn(data1,data2,data3,label =label, circle.col=col,plot = FALSE)
b$plot
dev.off()


names(b$table) <-gsub("A",label[1],names(b$table))
names(b$table) <-gsub("B",label[2],names(b$table))
names(b$table) <-gsub("C",label[3],names(b$table))


sets_1 <- names(b$table)
for (i in 1:length(sets_1)) {
  
  group <- sets_1[i]
  cres <- data.frame(b$table[[i]], group)
  #cres$Entrez_id <- row.names(cres)
  #cres <- merge(cres, ann, by.x = "Entrez_id", by.y = "Entrez_ID")
  write.xlsx(x = cres, file = paste0(flabel, ".venn.table.xlsx"), row.names = TRUE, append = TRUE)
}






###method2 不区分上下调 ,venn 图
library(venn)
library(VennDiagram)

### deglist 是一个存放差异基因的list, 只包含基因的名称或ID即可，list中数据的名称可以自定义，venn图可以显示自定义的名称。
cairo_pdf(str_c(flabel,".venn.pdf"),width=4,height=4)
venn(deglist,zcolor="style",ilcs=1.5,sncs=1.2,box=FALSE,borders=TRUE,opacity=0.5,ellipse = FALSE) #ilcs可以调整字体大小
dev.off()
res<-get.venn.partitions(deglist)

list <- list("common"=res$..values..[[1]],"after_only"=res$..values..[[2]],"before_only" = res$..values..[[3]])

write.xlsx(list,file = paste0(flabel, ".venn.table.xlsx"))






###method3 不区分上下调 ,大于5个sets, deglist为每个set组成的list。
library(UpSetR)
library(VennDiagram)



pdf(str_c(flabel,".venn.pdf"),width=4,height=4)



upset(fromList(deglist),  
      matrix.color = "gray30",
      main.bar.color = "gray30", 
      sets.bar.color = "lightskyblue",
      text.scale = 1.2,
      order.by = "freq", 
      point.size = 3, 
      line.size = 1,
      nsets=4,                  #设置显示的数量
      mb.ratio = c(0.5, 0.5),
      shade.alpha = 1, 
      matrix.dot.alpha = 1)
#      ,queries = list(list(query = intersects,          # 指定作用的交集
#                          params = list("GF", "GK", "GM", "GS"),color = "violet",active = T  ##设置颜色
#                      ),  
#                     list(query = intersects,
#                          params = list("GF"), color = "orange",
#                          active = T),
#                     list(query = intersects,
#                          params = list("GK"), color = "orange",
#                          active = T),
#                     list(query = intersects,
#                          params = list("GM"), color = "orange",
#                          active = T),
#                     list(query = intersects,
#                          params = list("GS"), color = "orange",
#			                            active = T))
#)

dev.off()

#lightskyblue

res<-get.venn.partitions(deglist)

write_tsv(res,paste0(flabel, ".venn.table.tsv"))

