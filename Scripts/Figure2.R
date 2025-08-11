#############Fig2#############
stat<-read.xlsx("Fig2A.xlsx")
lab<-names(sort(tapply(stat[,4],stat[,1],sum)))
p1<-ggplot(stat,aes(x=Cohort,y=Samples,fill=Heter))+
scale_fill_tableau()+
geom_bar(stat="identity",position="stack")+
theme_bw()+
scale_x_discrete(limits=lab,labels=lab)+
coord_flip()
p2<-ggplot(stat,aes(x=Cohort,y=Samples,fill=Code1))+
scale_fill_manual(values=cols2)+
geom_bar(stat="identity",position="stack")+
theme_bw()+
scale_x_discrete(limits=lab,labels=lab)+
coord_flip()
plt<-plot_grid(p1,p2)
ggsave("Fig2A.pdf",plt,height=6,width=10)
ggsave("Fig2A.png",plt,height=6,width=10)


#############
var<-read.xlsx("Fig2B.xlsx")
a<-dim(var)[1]
mycol<-pal_jco()(10)
mycol2<-mycol[1:a]
mycol2
num<-var[,2]
names(num)<-paste0(var[,1]," ",
round(var[,2]/sum(var[,2]),3)*100,"%"
)
pdf("Fig2B.pdf",height=5,width=5)
pie(num,border="black",col=mycol2,
clockwise=TRUE,angle=0)
dev.off()


#############
need1<-read.xlsx("Fig2C.xlsx")
dim(need1)
head(need1)
AFR<-unique(need1[need1$Ancestry=="AFR",]$value)
AMR<-unique(need1[need1$Ancestry=="AMR",]$value)
EAS<-unique(need1[need1$Ancestry=="EAS",]$value)
EUR<-unique(need1[need1$Ancestry=="EUR",]$value)
SAS<-unique(need1[need1$Ancestry=="SAS",]$value)
veen.plot<-venn.diagram(list(
AFR=AFR,
AMR=AMR,
EAS=EAS,
EUR=EUR,
SAS=SAS
),
width = 1000,height = 1000,
fill=c("#0073C2FF","#EFC000FF","#868686FF","#CD534CFF","#7AA6DCFF"), 
alpha=c(1,1,1,1,1), 
filename=NULL,
output=TRUE)
pdf("Fig2C.pdf",height=4,width=4)
grid.draw(veen.plot)
dev.off()


#############
need1<-read.xlsx("Fig2D.xlsx")
dim(need1)
head(need1)
AFR<-unique(need1[need1$Ancestry=="AFR",]$value)
AMR<-unique(need1[need1$Ancestry=="AMR",]$value)
EAS<-unique(need1[need1$Ancestry=="EAS",]$value)
EUR<-unique(need1[need1$Ancestry=="EUR",]$value)
SAS<-unique(need1[need1$Ancestry=="SAS",]$value)
veen.plot2<-venn.diagram(list(
AFR=AFR,
AMR=AMR,
EAS=EAS,
EUR=EUR,
SAS=SAS
),
width = 1000,height = 1000,
fill=c("#0073C2FF","#EFC000FF","#868686FF","#CD534CFF","#7AA6DCFF"), 
alpha=c(1,1,1,1,1), 
filename=NULL,
output=TRUE)
pdf("Fig2D.pdf",height=4,width=4)
grid.draw(veen.plot2)
dev.off()


#############
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_A1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_A2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2E.xlsx",sheet="pmat1_A",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2E.xlsx",sheet="col_A",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2E.xlsx",sheet="row_A",rowNames=T,colNames=T)
pdf("Fig2E_left.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#DA4C35"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors,
show_rownames=F,show_colnames=F)
dev.off()


#############
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_B1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_B2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2E.xlsx",sheet="pmat1_B",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2E.xlsx",sheet="col_B",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2E.xlsx",sheet="row_B",rowNames=T,colNames=T)
pdf("Fig2E_middle.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#4EB1C9"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors,
show_rownames=F,show_colnames=F)
dev.off()


#############
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_C1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_C2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2E.xlsx",sheet="pmat1_C",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2E.xlsx",sheet="col_C",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2E.xlsx",sheet="row_C",rowNames=T,colNames=T)
pdf("Fig2E_right.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#DCDD5F"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors,
show_rownames=F,show_colnames=F)
dev.off()


#############
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_A1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_A2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2F.xlsx",sheet="pmat1_A",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2F.xlsx",sheet="col_A",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2F.xlsx",sheet="row_A",rowNames=T,colNames=T)
pdf("Fig2F_left.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#DA4C35"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors)
dev.off()


################
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_B1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_B2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2F.xlsx",sheet="pmat1_B",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2F.xlsx",sheet="col_B",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2F.xlsx",sheet="row_B",rowNames=T,colNames=T)
pdf("Fig2F_middle.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#4EB1C9"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors)
dev.off()


################
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))(5)
ann_colors = list(
   Populations_C1= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]),
  Populations_C2= c("1"=myPalette[1],
"2"=myPalette[2],
"3"=myPalette[3],
"4"=myPalette[4],
"5"=myPalette[5]))
head(ann_colors)
pmat1<-read.xlsx("Fig2F.xlsx",sheet="pmat1_C",rowNames=T,colNames=T)
col_cli<-read.xlsx("Fig2F.xlsx",sheet="col_C",rowNames=T,colNames=T)
row_cli<-read.xlsx("Fig2F.xlsx",sheet="row_C",rowNames=T,colNames=T)
pdf("Fig2F_right.pdf",height=5,width=6.2)
pheatmap(as.matrix(pmat1),
col=colorRampPalette(c("white","#DCDD5F"))(10),
annotation_col=col_cli,
annotation_row=row_cli,
annotation_colors = ann_colors)
dev.off()

#############END#############