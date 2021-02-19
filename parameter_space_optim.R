library(ggplot2)
library(gridExtra)
library(grid)
library(ggjoy)
library(hrbrthemes)
library(deSolve)
library(foreign)
library(ggplot2)
library(reshape2)
library(ade4)
library(lattice)
library(scales)
library(Cairo)
library(grid)
library(akima)
library(rgl)
library(RColorBrewer)
library(directlabels)
library(TeachingDemos)
library(devtools)
library(contoureR)

source_url('https://gist.github.com/menugget/7689145/raw/dac746aa322ca4160a5fe66c70fec16ebe26faf9/image.scale.2.r')


setwd("C:/covid_vaccines")     ## set wd to the location of the data files

ar<-c(1/2500)
# ihr<-c(1,2,3)
delivery<-c(2)
trgt_doses<-seq(0.01,0.3,by=0.02)
dose_split<-c(1,0)
max_pd2<-seq(0.6,0.9,by=0.05)
d2_boost<-seq(0.5,1,by=0.05)
decay<-c(360)
# bis_boost<-c(1,0.95,0.9)
boost_interval<-seq(7*4,7*12,by=7)

# max_runs<-length(ar)*length(ihr)*length(trgt_doses)*length(dose_split)*length(max_pd2)*length(d2_boost)*length(decay)*length(bis_boost)*length(boost_interval)
max_runs<-length(ar)*length(trgt_doses)*length(dose_split)*length(max_pd2)*length(d2_boost)*length(boost_interval)*length(delivery)*length(decay)
variables<-8
runs<-matrix(0,nrow = max_runs,ncol = variables)
ind<-0
for (i in 1: length(ar)){
  for (h in 1:length(trgt_doses)){
    for (f in 1:length(dose_split)){
      for (w in 1:length(max_pd2)){
        for (q in 1:length(boost_interval)){
          for (k in 1:length(d2_boost)){
            for (t in 1:length(decay)){
              for (g in 1:length(delivery)){
                ind<-ind+1
                runs[ind,]<-c(ar[i],trgt_doses[h],dose_split[f],max_pd2[w],boost_interval[q],d2_boost[k],decay[t],delivery[g])
              }
            }
          }
        }
      }
    }
  }
}
df<-as.data.frame(runs)
dim(df)
colnames(df)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery")
write.table(df,file="parameters_split.csv",sep=",",row.names = F)


aa<-read.table("Run_all_split.txt",sep = ",")
res<-as.data.frame(aa)
colnames(res)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery",
                 "VPI_infection","VPI_clinical","VPI_death","1Dose","2Doses")

mda1<-res[which(res$Attack_rate==1/2500 & res$Delivery==2 & res$PD2==0.75 & res$Vacc_waning ==360 & res$Dose_split==0),]
mda2<-res[which(res$Attack_rate==1/2500 & res$Delivery==2 & res$PD2==0.75 & res$Vacc_waning ==360 & res$Dose_split==1),]

mda2<-mda2[order(mda2$Target_doses,mda2$Dose2_rel_boost),]
mda1<-mda1[order(mda1$Target_doses,mda1$Dose2_rel_boost),]
mda1$diff_clin<-mda2$VPI_clinical/mda1$VPI_clinical
int11t<-interp(mda1$Target_doses,y=mda1$Dose2_rel_boost,mda1$diff_clin, duplicate = "mean")

aa2<-read.table("Run_all_brazil.txt",sep = ",")
res2<-as.data.frame(aa2)
colnames(res2)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery",
                 "VPI_infection","VPI_clinical","VPI_death","1Dose","2Doses")

mda12<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.75 & res2$Vacc_waning ==360 & res2$Dose_split==0),]
mda22<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.75 & res2$Vacc_waning ==360 & res2$Dose_split==1),]

mda22<-mda22[order(mda22$Target_doses,mda22$Dose2_rel_boost),]
mda12<-mda12[order(mda12$Target_doses,mda12$Dose2_rel_boost),]
mda12$diff_clin<-mda22$VPI_clinical/mda12$VPI_clinical
int11t2<-interp(mda12$Target_doses,y=mda12$Dose2_rel_boost,mda12$diff_clin, duplicate = "mean")


aa3<-read.table("Run_all_uganda.txt",sep = ",")
res3<-as.data.frame(aa3)
colnames(res3)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery",
                  "VPI_infection","VPI_clinical","VPI_death","1Dose","2Doses")

mda13<-res3[which(res3$Attack_rate==1/2500 & res3$Delivery==2 & res3$PD2==0.75 & res3$Vacc_waning ==360 & res3$Dose_split==0),]
mda23<-res3[which(res3$Attack_rate==1/2500 & res3$Delivery==2 & res3$PD2==0.75 & res3$Vacc_waning ==360 & res3$Dose_split==1),]
mda23<-mda23[order(mda23$Target_doses,mda23$Dose2_rel_boost),]
mda13<-mda13[order(mda13$Target_doses,mda13$Dose2_rel_boost),]
mda13$diff_clin<-mda23$VPI_clinical/mda13$VPI_clinical
int11t3<-interp(mda13$Target_doses,y=mda13$Dose2_rel_boost,mda13$diff_clin, duplicate = "mean")


myPal <- colorRampPalette(c("midnightblue","deepskyblue3","darkseagreen2","cornsilk","khaki2","tan1","tomato1","red3"))
tiff("contour_clin_alloc.tiff", width = 30, height = 16, units = 'cm', res = 300)
# tiff("contour_clin_alloc_brazil.tiff", width = 25, height = 18, units = 'cm', res = 300)
layout(matrix(c(1,2,3,4), nrow=1, ncol=4), widths=c(5,4,4,1))
par(mar=c(5.5,4.5,3,0),cex.axis=1.25,cex.lab=1.5)
image(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="Dose Allocation - maximum number of doses available",xaxt="n", xlab="")
contour(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
title("UK")
box()
par(mar=c(5.5,0,3,0))
image(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="", yaxt="n", xaxt="n", xlab="")
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
title("Brazil")
box()
par(mar=c(5.5,0,3,0))
image(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="", yaxt="n", xaxt="n", xlab="")
contour(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
title("Uganda")
box()
par(mar=c(5.5,0,3,4.5))
image.scale(mda1$diff_clin, col=myPal(120), breaks=seq(0.6,1.8,by=0.01), axis.pos=4)
# axis(4,at=seq(0.7,1.7,by=0.1), labels=seq(0.7,1.7,by=0.1), las=2 )
grid.text("Efficacy of Single dose vs Double dose regimen", x = unit(0.5, "npc"), y = unit(0, "npc"),
          vjust = -0.8, gp = gpar(cex=1.15))
grid.text("Double dose vaccine effectiveness compared to single dose effectiveness ", x = unit(0.995, "npc"), y = unit(.5, "npc"), vjust = -0.06, rot = 90)
box()
dev.off()


myPal <- colorRampPalette(c("midnightblue","deepskyblue3","darkseagreen2","cornsilk","khaki2","tan1","tomato1","red3"))
tiff("contour_clin_alloc_vert.tiff", width = 22, height = 30, units = 'cm', res = 300)
layout(matrix(c(1,2,3,4), nrow=4, ncol=1), heights = c(1,4,4,4.5))
par(mar=c(0,4.5,4.5,2.5),cex.axis=1.25,cex.lab=1.5)
image.scale(mda1$diff_clin, col=myPal(120), breaks=seq(0.6,1.8,by=0.01), axis.pos=3)
# axis(4,at=seq(0.7,1.7,by=0.1), labels=seq(0.7,1.7,by=0.1), las=2 )
grid.text("Dose Allocation - maximum number of doses available", x = unit(0.02, "npc"), y = unit(0.5, "npc"),
          vjust = -0.2, gp = gpar(cex=1.15), rot = 90)
grid.text("Efficay of Single dose vs Double dose regimen", x = unit(0.5, "npc"), y = unit(0, "npc"),
          vjust = -0.2, gp = gpar(cex=1.15))
grid.text("Double dose vaccine effectiveness compared to single dose effectiveness ", y = unit(0.975, "npc"), x = unit(.5, "npc"), vjust = -0.06, rot = 0)
box()
par(mar=c(0,4.5,0,2.5))
image(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="",xaxt="n", xlab="")
contour(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
# title("UK")
box()
par(mar=c(0,4.5,0,2.5))
image(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="", xaxt="n", xlab="")
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
# title("Brazil")
box()
par(mar=c(3.5,4.5,0,2.5))
image(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),max(tail(int11t3$z),na.rm=T),ylab="",  xaxt="n", xlab="")
contour(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
# title("Uganda")
box()
grid.text("UK", x = unit(0.85, "npc"), y = unit(0.9, "npc"), rot = 0, gp = gpar(cex=1.5))
grid.text("Brazil", x = unit(.9, "npc"), y = unit(0.6, "npc"), gp = gpar(cex=1.5))
grid.text("Uganda", x = unit(.9, "npc"), y = unit(.3, "npc"), rot = 0, gp = gpar(cex=1.5))
dev.off()

require(MESS)
clines <- contourLines(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean"))
auc(clines[[4]]$x,clines[[4]]$y, type = 'spline')/0.27

clines <- contourLines(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean"))
auc(clines[[5]]$x,clines[[5]]$y, type = 'spline')/0.27

clines <- contourLines(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean"))
auc(clines[[7]]$x,clines[[7]]$y, type = 'spline',yleft=0.01)/0.27

library(bayestestR)
area_under_curve(clines[[7]]$x,clines[[7]]$y, method = "spline")/2.7
area_under_curve(1:10,rep(0.3,10))

mda1<-res[which(res$Attack_rate==1/2500 & res$Delivery==2 & res$PD2==0.7 & res$Vacc_waning ==360 & res$Dose_split==0),]
mda2<-res[which(res$Attack_rate==1/2500 & res$Delivery==2 & res$PD2==0.7 & res$Vacc_waning ==360 & res$Dose_split==1),]
mda2<-mda2[order(mda2$Target_doses,mda2$Dose2_rel_boost),]
mda1<-mda1[order(mda1$Target_doses,mda1$Dose2_rel_boost),]
mda1$diff_clin<-mda2$VPI_clinical/mda1$VPI_clinical
int11t<-interp(mda1$Target_doses,y=mda1$Dose2_rel_boost,mda1$diff_clin, duplicate = "mean")

aa2<-read.table("Run_all_brazil.txt",sep = ",")
res2<-as.data.frame(aa2)
colnames(res2)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery",
                  "VPI_infection","VPI_clinical","VPI_death","1Dose","2Doses")

mda12<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.7 & res2$Vacc_waning ==360 & res2$Dose_split==0),]
mda22<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.7 & res2$Vacc_waning ==360 & res2$Dose_split==1),]
mda22<-mda22[order(mda22$Target_doses,mda22$Dose2_rel_boost),]
mda12<-mda12[order(mda12$Target_doses,mda12$Dose2_rel_boost),]
mda12$diff_clin<-mda22$VPI_clinical/mda12$VPI_clinical

aa3<-read.table("Run_all_uganda.txt",sep = ",")
res3<-as.data.frame(aa3)
colnames(res3)<-c("Attack_rate","Target_doses","Dose_split","PD2","Boost_interval","Dose2_rel_boost","Vacc_waning","Delivery",
                  "VPI_infection","VPI_clinical","VPI_death","1Dose","2Doses")

mda13<-res3[which(res3$Attack_rate==1/2500 & res3$Delivery==2 & res3$PD2==0.7 & res3$Vacc_waning ==360 & res3$Dose_split==0),]
mda23<-res3[which(res3$Attack_rate==1/2500 & res3$Delivery==2 & res3$PD2==0.7 & res3$Vacc_waning ==360 & res3$Dose_split==1),]
mda23<-mda23[order(mda23$Target_doses,mda23$Dose2_rel_boost),]
mda13<-mda13[order(mda13$Target_doses,mda13$Dose2_rel_boost),]
mda13$diff_clin<-mda23$VPI_clinical/mda13$VPI_clinical


coul=c("#E8421D","#FFD423","#11B9E8","#981FFF","#4AF072","#FC6B13","#2B7BD6","#EC2DF2")

### contour lines  = 1
tiff("contour1_countries.tiff", width = 16, height = 16, units = 'cm', res = 300)
par(mfrow=c(1,1))
contour(interp(mda1$Dose2_rel_boost,y=mda1$Target_doses,mda1$diff_clin, duplicate = "mean", linear = T, extrap = T), ylim = c(0.02,0.28925),xlim = c(0.5185,0.9815),
        add=F,drawlabels =F, levels=1, lwd=2.5, col = coul[6],xlab="Efficacy of Single dose vs Double dose regimen", ylab="Dose Allocation - maximum number of doses available")
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean", linear = T, extrap = T),
        add=T,drawlabels =F, levels = 1, lwd=2.5, col =coul[4])
contour(interp(mda13$Dose2_rel_boost,y=mda13$Target_doses,mda13$diff_clin, duplicate = "mean", linear = T, extrap = T),
        add=T,drawlabels =F, levels = 1, lwd=2.5, col =coul[3])
legend("topleft", legend=c("UK", "Brazil","Uganda"), box.col = "white",
       col=coul[c(6,4,3)], lwd=2.5,lty=1, cex=0.8)
box()
dev.off()


mda12<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.75 & res2$Vacc_waning ==360 & res2$Dose_split==0),]
mda22<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.75 & res2$Vacc_waning ==360 & res2$Dose_split==1),]
mda22<-mda22[order(mda22$Target_doses,mda22$Dose2_rel_boost),]
mda12<-mda12[order(mda12$Target_doses,mda12$Dose2_rel_boost),]
# mda12$diff_clin<-mda22$VPI_clinical/mda12$VPI_clinical

image(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$VPI_clinical, duplicate = "mean"),col=c(myPal(119),"red3"),breaks=seq(0.6,1.8,by=0.01),ylab="",xaxt="n", xlab="")
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$VPI_clinical, duplicate = "mean"),add=T,labcex=0.8)
contour(interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$VPI_clinical, duplicate = "mean"),add=T,levels=1, labcex=0, lwd=2.5)
axis(1, at = d2_boost[seq(3, 11, by = 2)], d2_boost[seq(3, 11, by = 2)])
# title("UK")
box()


# ii<-interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean")
# x=ii$x; y=ii$y ;z=ii$z
# zz = expand.grid(x=x,y=y); zz$zz = c(z) 
# df75 = getContourLines(zz,levels=1)
# 
# mda12<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.6 & res2$Vacc_waning ==360 & res2$Dose_split==0.5),]
# mda22<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.6 & res2$Vacc_waning ==360 & res2$Dose_split==1),]
# mda22<-mda22[order(mda22$Target_doses,mda22$Dose2_rel_boost),]
# mda12<-mda12[order(mda12$Target_doses,mda12$Dose2_rel_boost),]
# mda12$diff_clin<-mda22$VPI_clinical/mda12$VPI_clinical
# 
# ii<-interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean")
# x=ii$x; y=ii$y ;z=ii$z
# zz = expand.grid(x=x,y=y); zz$zz = c(z) 
# df65 = getContourLines(zz,levels=1)
# 
# mda12<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.9 & res2$Vacc_waning ==360 & res2$Dose_split==0),]
# mda22<-res2[which(res2$Attack_rate==1/2500 & res2$Delivery==2 & res2$PD2==0.9 & res2$Vacc_waning ==360 & res2$Dose_split==1),]
# mda22<-mda22[order(mda22$Target_doses,mda22$Dose2_rel_boost),]
# mda12<-mda12[order(mda12$Target_doses,mda12$Dose2_rel_boost),]
# mda12$diff_clin<-mda22$VPI_clinical/mda12$VPI_clinical
# 
# ii<-interp(mda12$Dose2_rel_boost,y=mda12$Target_doses,mda12$diff_clin, duplicate = "mean")
# x=ii$x; y=ii$y ;z=ii$z
# zz = expand.grid(x=x,y=y); zz$zz = c(z) 
# df85 = getContourLines(zz,levels=1)
# 
# DF<-rbind(cbind(cbind(df65$x,df65$y),rep(65,length(df65$z))), 
#             cbind(cbind(df75$x,df75$y),rep(75,length(df75$z))),
#                   cbind(cbind(df85$x,df85$y),rep(85,length(df85$z))))
# 
# DF<-as.data.frame(DF)
# colnames(DF)<-c("Dose 1 relative efficacy","Allocation","PD2 efficacy")
# g<-ggplot(DF,aes(x=`Dose 1 relative efficacy`,y=Allocation, group=`PD2 efficacy`))+
#   geom_line(aes(color=as.factor(`PD2 efficacy`)))+  
#   scale_fill_manual(values=myPal(3))+theme_minimal()
# g
