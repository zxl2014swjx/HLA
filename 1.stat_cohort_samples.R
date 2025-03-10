setwd("D:/3_2024_Human_Immunity/Analysis/1.Landscape")

var<-read.xlsx("Cohort_samples.xlsx",sheet=1)
colnames(var)<-c("Cohorts","Involved Samples","Accessible Samples")
var<-var[order(var[,2],decreasing=T),]
label<-as.vector(var[,1])

var1<-melt(var,id.vars=1)
colnames(var1)<-c("Cohorts","Type","Samples")


p1<-ggplot(var1,aes(x=Cohorts,y=Samples,fill=Type))+
geom_bar(stat="identity",position="dodge")+
theme_bw()+
scale_y_break(c(5000, 8000),scales="free")+
theme(legend.position="none",
axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1)
)+
scale_fill_npg()+
geom_text(aes(label = var1$Samples), size=2,
position=position_stack(vjust = 1))+
facet_wrap(.~Type)

ggsave("Bar_Cohorts_Samples.pdf",p1,height=4,width=7)
ggsave("Bar_Cohorts_Samples.png",p1,height=4,width=7)



###############
setwd("D:/3_2024_Human_Immunity/Analysis/1.Landscape")


data<-read.xlsx("Cohort_samples.xlsx",sheet=2)

var<-as.data.frame(table(data[,3]))

a<-dim(var)[1]
mycol<-pal_jco()(10)
mycol2<-mycol[1:a]
mycol2

num<-var[,2]
names(num)<-paste0(var[,1]," ",
round(var[,2]/sum(var[,2]),3)*100,"%"
)


pdf("Pie_Population.pdf",height=5,width=5)
pie(num,border="black",col=mycol2,
clockwise=FALSE,angle=0)
dev.off()


##########################