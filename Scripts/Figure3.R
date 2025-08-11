#############Fig3#############
test<-read.xlsx("Fig3A.xlsx",sheet=1,rowNames=T)
test[1:4,1:4]
var<-c("A","B","C")
anno<-read.xlsx("Fig3A.xlsx",sheet=2)
li<-list()
for(i in 1:3){
test1<-test[,grep(paste0("_",var[i]),colnames(test))]
colnames(test1)<-gsub(paste0("_",var[i]),"",colnames(test1))
Laura <- phyDat(test1,type="AA")
tree<-pratchet(Laura)
tree_acc <- acctran(tree, Laura)
li[[i]]<-ggtree(tree_acc,#ladderize=FALSE,
layout='fan',open.angle=180,
branch.length="none"
)+
geom_tiplab(size=4,hjust=-0.5)+
geom_nodepoint(color='#b5e521', alpha=1/4, size=5)+
geom_tippoint(color='#FDAC4F', shape=8, size=3)+
geom_fruit(data=anno,geom=geom_star,
mapping=aes(y=ID, fill=Type, size=Num, starshape=Type),
position="identity",starstroke=0.2)+
scale_size_continuous(
range=c(1, 5),
guide=guide_legend(keywidth=0.5,Keyheight=0.5,
override.aes=list(starshape=15),order=2)) +
scale_fill_jco() +
theme(legend.position="top")+
guides(fill=guide_legend(nrow=1))
}
plt<-plot_grid(li[[1]],li[[2]],li[[3]])
ggsave(paste0("Tree_Code1_",var[i],".pdf"),plt,height=5,width=12)
ggsave(paste0("Tree_Code1_",var[i],".png"),plt,height=5,width=12)


#############
mat<-read.xlsx("Fig3B.xlsx",sheet=1)
anno<-read.xlsx("Fig3B.xlsx",sheet=2)
Laura <- phyDat(mat,type="AA")
tree<-pratchet(Laura)
tree_acc <- acctran(tree, Laura)
p<-ggtree(tree_acc,#ladderize=FALSE,
layout='circular',open.angle=180,
branch.length="none"
)+
geom_tiplab(size=4,hjust=-0.5)+
geom_nodepoint(color='#b5e521', alpha=1/4, size=5)+
geom_tippoint(color='#FDAC4F', shape=8, size=3)+
geom_fruit(data=anno,geom=geom_star,
mapping=aes(y=ID, fill=Type, size=Num, starshape=Type),
position="identity",starstroke=0.2)+
scale_size_continuous(
range=c(1, 5),
guide=guide_legend(keywidth=0.5,Keyheight=0.5,
override.aes=list(starshape=15),order=2)) +
scale_fill_jco() +
theme(legend.position="right")+
guides(fill=guide_legend(ncol=1))
p_code2
ggsave("Fig3B.pdf",p,height=5,width=5)
ggsave("Fig3B.png",p,height=5,width=5)


#############
test<-read.csv("Fig3C.xlsx",sheet=1,rowNames=T)
test[1:4,1:4]
var1<-c("A","B","C")
anno<-read.xlsx("Fig3C.xlsx",sheet=2)
anno$col<-anno$ID
var2<-as.data.frame(table(anno$Type))
for(i in 1:3){
test1<-test[,grep(paste0("_",var1[i]),colnames(test))]
colnames(test1)<-gsub(paste0("_",var1[i],"2_aa"),"",colnames(test1))
li<-list()
for(j in 1:dim(var2)[1]){
anno1<-anno[anno[,2]==var2[j,1],]
col<-anno1[,1]
test2<-test1[,col]
Laura <- phyDat(test2,type="AA")
tree<-pratchet(Laura)
tree_acc <- acctran(tree, Laura)
li[[j]]<-ggtree(tree_acc,#ladderize=FALSE,
layout='dendrogram',branch.length="none"
)+
geom_tiplab(size=4,vjust=2,hjust=0.5)+
geom_nodepoint(color='#b5e521', alpha=1/4, size=5)+
geom_tippoint(color='#FDAC4F', shape=8, size=3)+
geom_fruit(data=anno,geom=geom_star,
mapping=aes(y=ID, fill=col,size=Num, starshape=col),
position="identity",starstroke=0.2)+
scale_size_continuous(
range=c(1, 5),
guide=guide_legend(keywidth=0.5,Keyheight=0.5,
override.aes=list(starshape=15),order=2)) +
theme(legend.position="right")+
guides(fill=guide_legend(ncol=1))+
ggtitle(paste0(as.vector(var2[j,1]),"_",var1[i]))+
theme(plot.title=element_text(hjust=0.5))
}
p3<-plot_grid(li[[1]],li[[2]],li[[3]],
li[[4]],li[[5]],
nrow=5)
ggsave(paste0("Tree_Code2_split_",var1[i],".pdf"),p3,height=10,width=3)
ggsave(paste0("Tree_Code2_split_",var1[i],".png"),p3,height=10,width=3)
}

#############END#############