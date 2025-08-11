#############Fig4#############
data<-read.xlsx("Fig4A.xlsx")
data1<-data[data$Samples>0,]
pdf("Fig4A.pdf")
hchinamap(name = data1$name,
value = data1$Samples, region = "China",
minColor="#E7FBFF",maxColor="#93C4DE",
title="CAS",
theme = "sunset")
dev.off()


#############
data<-read.xlsx("Fig4B.xlsx")
data1<-data[data$Samples>0,]
pdf("Fig4B.pdf")
hchinamap(name = data1$name,
value = data1$Samples, region = "China",
minColor="#E7FBFF",maxColor="#08306B",
title="STS",
theme = "sunset")
dev.off()


############
cor_mat<-c("Fig4C.xlsx",sheet=1,rowNames=T,colNames=T)
cor_sig<-c("Fig4C.xlsx",sheet=2,rowNames=T,colNames=T)
pdf("Fig4C_upper.pdf",height=7,width=7)
corrplot(as.matrix(cor_mat),
p.mat = sig_mat,
insig ='label_sig',
sig.level = c(0.001,0.01,0.05),
tl.cex=1,tl.col="black",
pch.cex =1,pch.col ="black",
method="square",
type="upper",
#order="AOE",
hclust.method ="complete",
is.corr=TRUE,
col.lim=c(-1,1),
col=rev(COL2('RdBu',100)),
)
dev.off()
pdf("Fig4C_lower.pdf",height=7,width=7)
corrplot(as.matrix(cor_mat),
tl.cex=1,tl.col="black",
#method="pie",
method="number",
type="lower",
#order="AOE",
hclust.method ="complete",
is.corr=TRUE,
col.lim=c(-1,1),
col=rev(COL2('RdBu',100)),
)
dev.off()


############
sig1<-read.xlsx("Fig4D.xlsx")
pmat<-dcast(sig1,row~col,value.var='PCC')
rownames(pmat)<-as.vector(pmat[,1])
pmat<-pmat[,-1]
head(pmat)
dim(pmat)
psig<-dcast(sig1,row~col,value.var='format')
rownames(psig)<-as.vector(psig[,1])
psig<-psig[,-1]
head(psig)
dim(psig)
bk1<-seq(min(pmat),0,by=0.01)
bk2<-seq(0,max(pmat),by=0.01)
bk<-c(bk1,bk2)
col_sel<-c(colorRampPalette(colors=c("blue","white"))(which(bk==0)),
colorRampPalette(colors=c("white","red"))(length(bk)-which(bk==0)))
library(pheatmap)
pdf("Fig4D.pdf",height=9,width=6)
pheatmap(as.matrix(pmat),
display_numbers = as.matrix(psig),
#cluster_rows=F,cluster_cols=F,
cellheight=12,cellwidth=12,fontsize=10,
color=col_sel,breaks=bk)
dev.off()


############
stat<-read.xlsx("Fig4E.xlsx")
a<-apply(bak,2,sum)
labels<-c(paste0("Age[20,30)\n",a[1]),
paste0("Age[30,35)\n",a[2]),
paste0("Age[35,40)\n",a[3]),
paste0("Age[40,45)\n",a[4]),
paste0("Age[45,70)\n",a[5])
)
p1<-ggplot(stat,aes(x=AgeGroup,y=Percentage,fill=Disease))+
geom_bar(stat="identity",position="fill",width=0.8)+
#scale_fill_npg()+
scale_fill_manual(values=c("#4DBBD5FF","#E64B35FF"))+
theme_bw()+
ggtitle(paste0("Fisher Exact Test\np = ",
format(p.val,scientific=TRUE,digit=3)))+
xlab("")+ylab("Percentage")+
geom_text(label = paste0(round(stat$Percentage*100,1),"%"), 
size=4,position=position_stack(vjust = 0.5))+
scale_x_discrete(limits=c(1,2,3,4,5),
labels=labels)+
scale_y_continuous(expand=c(0,0))+
#guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
theme(plot.title = element_text(size=15,hjust = 0.5),
)
ggsave("Fig4E.pdf",p1,height=6,width=6)
ggsave("Fig4E.png",p1,height=6,width=6)


##############
all1<-read.xlsx("Fig4F.xlsx")
comp<-list(c("Within","Without"))
stat1<-compare_means(value~Disease,all1,group.by="variable",method="t.test")$p
text.format<-round(stat1,3)
p<-ggboxplot(all1, x="variable", 
y="value",#bxp.errorbar=TRUE,
fill = "Disease",
add=c("boxplot"),
palette=c("#4DBBD5FF","#E64B35FF"),
)+
theme_bw()+xlab("")+ylab("Normalization Value")+
#facet_wrap(.~variable,nrow=1)+
stat_compare_means(comparisons = comp,method="t.test",
method.args = list(alternative = "two.sided")
)+
#theme(legend.position='none')+
annotate("text", x = c(1:6), y = 1.2, size = 4, 
colour = "black",label=text.format)
ggsave("Fig4F.pdf",p,height=3,width=5)
ggsave("Fig4F.png",p,height=3,width=5)

#############END#############