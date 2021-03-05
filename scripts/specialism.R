# One --------------------

#mammal supertree
library(ape)
treelist=read.nexus("./data/original/Park_2016/WR_2005_ST.txt")
#best dates
mtree=treelist[[1]]
#dist matrix
phydist<-cophenetic.phylo(mtree) #or can use generic cophenetic
#which hosts are in the parasite database
d<-read.csv("./data/original/Park_2016/GMPD_main_2016-11-28.csv",header=T)
d<-subset(d,HostEnvironment=="terrestrial")
keep.d<-which(names(d)%in%c("HostCorrectedName","HostOrder","ParasiteCorrectedName","Par.Type","Group"))
d<-d[,keep.d]
#remove rows where host is not identified to species
d<-d[-which(d$HostCorrectedName=="no binomial name"),]
#remove rows where parasite is not identified to species
d<-d[-which(d$ParasiteCorrectedName=="no binomial name"),]
d<-d[-which(d$ParasiteCorrectedName=="not identified to genus"),]
#Sys.setlocale('LC_ALL','C') 
d<-d[-grep("sp\\.",d$ParasiteCorrectedName),]
## merge transmission mode data
ptrans<-read.csv("./data/original/Park_2016/GMPD_parasite_traits_2016-11-28.csv",header=T)
ptrans<-subset(ptrans,select=c("ParasiteCorrectedName","close","nonclose","vector","intermediate"))
d<-merge(d,ptrans,by="ParasiteCorrectedName")
# identify host species in gmpd
obs.hosts<-unique(d$HostCorrectedName)
obs.hosts<-gsub(" ","_",obs.hosts) #put in underscore for compatibility with other dataframe
#at which locations of phydist do these hosts occur?
idx<-which(unlist(dimnames(phydist)[1]) %in% obs.hosts)
#pull out smaller cophenetic matrix of only these hosts
phydist.mini<-phydist[idx,idx]
#make a community matrix for the call to ses.mpd
d$HostCorrectedName<-gsub(" ","_",d$HostCorrectedName)
d$HostCorrectedName<-as.factor(d$HostCorrectedName)
d<-droplevels(d)
comm<-as.data.frame.matrix(table(d[,c(which(names(d)=="ParasiteCorrectedName"),which(names(d)=="HostCorrectedName"))]))
#comm<-1*(comm>0)
#get normalized z-score for mpd
library(picante)
phydist.mini<-phydist.mini[order(rownames(phydist.mini)),order(rownames(phydist.mini))]

# Two -------------------------------

nri<-ses.mpd(comm,phydist.mini,null.model="independentswap",runs=10,abundance.weighted=T)#change back to 1000!

#load("get_nri.Rda") #nri<-ses.mpd(comm,phydist.mini,null.model="independentswap",runs=1000,abundance.weighted=T)
load("./data/original/Park_2016/get_nriABUND.Rda") #nri<-ses.mpd(comm,phydist.mini,null.model="independentswap",runs=1000)

# prep to merge with other dataframe that has other parasite traits
#nri<-nri[complete.cases(nri),]  # THIS REMOVES SINGLETON PARASITES (ONLY ONE HOST) SINCE MPD STUFF IS NA
nri$ParasiteCorrectedName<-rownames(nri)
nri<-merge(nri,ptrans,by="ParasiteCorrectedName")

ptype<-read.csv("./data/original/Park_2016/GMPD_parasite_taxonomy_2016-11-28.csv",header=T)
ptype<-subset(ptype,select=c("ParasiteCorrectedName","ParType"))
ptype<-ptype[!duplicated(ptype),]
nri<-merge(nri,ptype,by="ParasiteCorrectedName")

#remove rare parasite types
nri4<-nri[-which(nri$ParType %in% c("Fungus","Prion")),]#c("Fungus","Prion")

nri4$n.modes<-rowSums(cbind(nri4$close,nri4$nonclose,nri4$vector,nri4$intermediate))
nri6<-nri4
names(nri6)[which(names(nri6)=="ParasiteCorrectedName")]<-"para.name"
names(nri6)[which(names(nri6)=="ParType")]<-"para.type"
nri6$para.name<-as.character(nri6$para.name)
nri6<-nri6[complete.cases(nri6),]

nri.flat<-data.frame(para.name=character(),ntaxa=integer(),mpd.obs.z=numeric(),mpd.obs.p=numeric(),para.type=character(),tmode=character(),stringsAsFactors=FALSE)
k<-1
for (i in 1:dim(nri6)[1]){
  if (nri6[i,"close"]==1){nri.flat[k,"para.name"]<-nri6[i,"para.name"];nri.flat[k,"ntaxa"]<-nri6[i,"ntaxa"];nri.flat[k,"mpd.obs.z"]<-nri6[i,"mpd.obs.z"];nri.flat[k,"mpd.obs.p"]<-nri6[i,"mpd.obs.p"];nri.flat[k,"para.type"]<-as.character(nri6[i,"para.type"]);nri.flat[k,"tmode"]<-"close";k<-k+1}
  if (nri6[i,"nonclose"]==1){nri.flat[k,"para.name"]<-nri6[i,"para.name"];nri.flat[k,"ntaxa"]<-nri6[i,"ntaxa"];nri.flat[k,"mpd.obs.z"]<-nri6[i,"mpd.obs.z"];nri.flat[k,"mpd.obs.p"]<-nri6[i,"mpd.obs.p"];nri.flat[k,"para.type"]<-as.character(nri6[i,"para.type"]);nri.flat[k,"tmode"]<-"nonclose";k<-k+1}
  if (nri6[i,"vector"]==1){nri.flat[k,"para.name"]<-nri6[i,"para.name"];nri.flat[k,"ntaxa"]<-nri6[i,"ntaxa"];nri.flat[k,"mpd.obs.z"]<-nri6[i,"mpd.obs.z"];nri.flat[k,"mpd.obs.p"]<-nri6[i,"mpd.obs.p"];nri.flat[k,"para.type"]<-as.character(nri6[i,"para.type"]);nri.flat[k,"tmode"]<-"vector";k<-k+1}
  if (nri6[i,"intermediate"]==1){nri.flat[k,"para.name"]<-nri6[i,"para.name"];nri.flat[k,"ntaxa"]<-nri6[i,"ntaxa"];nri.flat[k,"mpd.obs.z"]<-nri6[i,"mpd.obs.z"];nri.flat[k,"mpd.obs.p"]<-nri6[i,"mpd.obs.p"];nri.flat[k,"para.type"]<-as.character(nri6[i,"para.type"]);nri.flat[k,"tmode"]<-"intermediate";k<-k+1}
}

nri.prot <- nri6 %>% filter(para.type == "Protozoa") %>% select(protname = para.name, starts_with("mpd"), ntaxa, n.modes)

allprots <- read.csv("./data/modified/allprots.csv")[-1]
nri.prot <- left_join(allprots, nri.prot)

write.csv(nri.prot, "./data/modified/mpd_prot.csv")


# # Fig 1 -------------------------------- 
# 
# library(ggplot2)
# nri.flat$para.type<-as.factor(nri.flat$para.type)
# nri.flat$tmode<-as.factor(nri.flat$tmode)
# nri.flat<-nri.flat[complete.cases(nri.flat),]
# #order factors in z score order for plotting
# ag.para.z<-aggregate(nri.flat$mpd.obs.z,by=list(nri.flat$para.type),FUN=mean)
# ag.para.z<-ag.para.z[order(ag.para.z$x,decreasing=T),]
# nri.flat$para.type<-factor(nri.flat$para.type,ag.para.z$Group.1)
# ag.tmode.z<-aggregate(nri.flat$mpd.obs.z,by=list(nri.flat$tmode),FUN=mean)
# ag.tmode.z<-ag.tmode.z[order(ag.tmode.z$x,decreasing=F),]
# nri.flat$tmode<-factor(nri.flat$tmode,ag.tmode.z$Group.1)
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# global.median.z<-median(nri6$mpd.obs.z,na.rm=T)
# 
# ggplot(nri.flat, aes(x=para.type, y=mpd.obs.z, fill=tmode)) + geom_boxplot(notch=T) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + scale_fill_discrete(name="Transmission\nmode",labels=c("Close","Vector","Complex","Envir"))+xlab("Parasite type")+ylab("Standard effect size of mean pairwise PD")+ylim(c(-4,1))+geom_hline(yintercept=global.median.z,colour="black")
# 
# #explanation for horn-shaped boxes: http://r.789695.n4.nabble.com/Strange-horns-on-notched-box-plots-td858004.html
# 
# 
# sig.z<-NULL #note all significant z scores are negative (checked independently)
# for (i in unique(nri6$para.type)){
#   tmp<-subset(nri6,para.type==i)
#   sig.z<-rbind(sig.z,c(i,dim(tmp)[1],length(which(tmp$mpd.obs.p<0.05))))
# }
# sig.z<-as.data.frame(sig.z)
# names(sig.z)<-c("para.type","n","x")
# sig.z$n<-as.integer(as.character(sig.z$n))
# sig.z$x<-as.integer(as.character(sig.z$x))
# sig.z$y<-sig.z$n-sig.z$x
# sig.z$p<-sig.z$x/sig.z$n
# #prop.test(sig.z$x,sig.z$n)
# sig<-NULL
# for (i in 1:dim(sig.z)[1]){
#   for(j in 1:sig.z[i,"x"]){
#     sig<-rbind(sig,c(as.character(sig.z$para.type[i]),1))
#   }
#   for(j in 1:sig.z[i,"y"]){
#     sig<-rbind(sig,c(as.character(sig.z$para.type[i]),0))
#   }
# }
# sig<-as.data.frame(sig)
# names(sig)<-c("para.type","sig")
# sig$para.type<-factor(sig$para.type,levels=sig.z[order(sig.z$p),]$para.type)
# n4taxa<-table(sig$para.type)
# 
# sig.a<-ggplot(sig,aes(x=para.type,fill=sig))+geom_bar(position="fill")+scale_fill_discrete(name="Significant")+xlab("Parasite type")+ylab("")+scale_fill_manual(values=c("gray","dodgerblue4"))+coord_flip()+theme(legend.position="none")+annotate("text",x=5,y=0.5,label=paste("n =",as.numeric(n4taxa[5])))+annotate("text",x=4,y=0.5,label=paste("n =",as.numeric(n4taxa[4])))+annotate("text",x=3,y=0.5,label=paste("n =",as.numeric(n4taxa[3])))+annotate("text",x=2,y=0.5,label=paste("n =",as.numeric(n4taxa[2])))+annotate("text",x=1,y=0.5,label=paste("n =",as.numeric(n4taxa[1])))
