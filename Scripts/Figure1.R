#############Fig1#############
var<-read.xlsx("Fig1B.xlsx")
a<-dim(var)[1]
mycol<-brewer.pal(12,'Set3')
mycol2<-colorRampPalette(mycol)(a)
mycol2
num<-var[,2]
names(num)<-paste0(var[,1]," ",
round(var[,2]/sum(var[,2]),3)*100,"%"
)
pdf("Fig1B.pdf",height=8,width=8)
pie(num,border="black",col=mycol2,
clockwise=FALSE,angle=0)
dev.off()


#############
data2<-read.xlsx("Fig1C.xlsx")
my_comparisons<-list(c("HLA_A","HLA_B"),
c("HLA_B","HLA_C"),c("HLA_A","HLA_C"))
p<-ggviolin(data2, x="HLA", 
y="HED",#bxp.errorbar=TRUE,
fill = "HLA", 
add=c("boxplot","median"),
)+
theme_bw()+
scale_fill_manual(values=c("#DA4C35","#4EB1C9","#DCDD5F"))+
stat_compare_means(comparisons = my_comparisons)+
theme(legend.position='none')
ggsave("Fig1C.pdf",p,height=5,width=5)
ggsave("Fig1C.png",p,height=5,width=5)


#############
data2<-read.xlsx("Fig1D.xlsx")
my_comparisons <- list(c("HLA_A","HLA_B"),
c("HLA_B","HLA_C"),c("HLA_A","HLA_C"))
stat<-compare_means(HED~HLA,data2,group.by="Cohort",method="t.test")
table(stat$p.signif)
stat$com<-paste0(stat$group1,"_",stat$group2)
format<-dcast(stat,Cohort~com,value.var="p.signif")
label<-unique(sort(all$Cohort))
lab<-data.frame(Inx=1:length(label),Cohort=label)
need<-merge(lab,format,by="Cohort")
need<-need[order(need$Inx),c(1,2,4,3,5)]
need<-need[order(need$Cohort),]
text.format<-paste0(need[,4],"\n",need[,5],"\n",need[,3])
p<-ggboxplot(data2, x="Cohort", 
y="HED",#bxp.errorbar=TRUE,
fill = "HLA", width=0.4,
add=c("mean"),
palette="npg"
)+
theme_bw()+
xlab("")+ylab("")+
theme_bw()+
stat_compare_means(comparisons = my_comparisons)+
scale_x_discrete(limits=label,labels=label)+
theme(legend.position="top",
text=element_text(hjust = 0.5),
axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
plot.title=element_text(hjust = 0.5))+
scale_fill_manual(values=c("#DA4C35","#4EB1C9","#DCDD5F"))+
annotate("text", x = c(1:24),  y = 14.8, size = 3, 
colour = "red",label=text.format)+
scale_x_discrete(limits=label,labels=label)
ggsave("Fig1D.pdf",p,height=5,width=10)
ggsave("Fig1D.png",p,height=5,width=10)


#############
stat2<-read.xlsx("Fig1E.xlsx")
stat2<-stat2[order(stat2$Freq,decreasing=T),]
stat2<-stat2[order(stat2$Group),]
p<-ggplot(stat2,aes(x=Allele,y=Freq,fill=Group))+
geom_bar(stat="identity",position="dodge")+
scale_x_discrete(limits=stat2$Allele)+
#scale_fill_npg()+
scale_fill_manual(values=c("#DA4C35","#4EB1C9","#DCDD5F"))+
theme_bw()+
theme(legend.position="none",
axis.text.x=element_text(vjust=0.5,hjust=1,angle=90))+
geom_text(label=paste0(round(stat2$Freq,3)*100,"%"),
angle=90)
ggsave("Fig1E.pdf",p,height=5,width=6)
ggsave("Fig1E.png",p,height=5,width=6)


#############
stat2<-read.xlsx("Fig1F.xlsx")
stat2<-stat2[order(stat2$Freq,decreasing=T),]
stat2<-stat2[order(stat2$Group),]
p<-ggplot(stat2,aes(x=Allele,y=Freq,fill=Group))+
geom_bar(stat="identity",position="dodge")+
scale_x_discrete(limits=stat2$Allele)+
#scale_fill_npg()+
scale_fill_manual(values=c("#DA4C35","#4EB1C9","#DCDD5F"))+
theme_bw()+
xlab("Allele pairs")+
theme(legend.position="none",
axis.text.x=element_text(vjust=0.5,hjust=1,angle=90))+
geom_text(label=paste0(round(stat2$Freq,3)*100,"%"),
angle=90)
ggsave("Fig1F.pdf",p,height=5,width=6)
ggsave("Fig1F.png",p,height=5,width=6)

#############END#############


