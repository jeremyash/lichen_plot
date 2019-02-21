#### CL barplot ####
rb<-colorRampPalette(c("yellow","red","darkred"))
rb(5)
gb<-colorRampPalette(c("green","blue","darkblue"))
gb(5)
#### N_CL ####
SNtb<-SNt[NS_.best.model==1|N.best.model==1]
spcds.N<-SNtb[spcd%in%cls$spcd,min(Npmr_1,na.rm=T),by=c("spcd","response","max_N","min_N","GENUS","SPECIES")][order(-V1)]
if(nrow(spcds.N)==0){
  spcds.N<-cls[,c("spcd","GENUS","SPECIES")]
  spcds.N[,(c("response","max_N","min_N","V1")):=NA]
} 
spcds.N[,(grep("Npmr_|Npmr..si",names(SNtb),value = T)):=SNtb[match(spcds.N[,paste(spcd,response,V1)],paste(spcd,response,Npmr_1)),grep("Npmr_|Npmr..si",names(SNtb),value = T),with=F]]
spcds<-spcds.N[,min(Npmr_1,na.rm=T),by=c("spcd","GENUS","SPECIES")][order(-V1,GENUS,SPECIES),spcd]
spcds<-spcds[spcds%in%park.veg.sub$spcd]
#spcds.N[,grep("Npmr_|Npmr..si",names(SNtb),value = T),with=F]>spcds.N[,max_N]
par(fig=c(0,.5,0,1),mar=c(3,0,0,2),oma=c(0,3,6,1),new=F)
plot(x = c(-5,max(30,max(eco.Ndep.val)*1.1,na.rm=T)),y = c(1,-max(57,length(spcds))),axes=F,ylab="",xlab="",type="n")
#mtext("tree species symbol",side=2,outer=F,cex=1,at=-length(spcds)/2)
text(-.2,-c(1:length(spcds)),spcd.ref[match(spcds,SPCD),SPECIES_SYMBOL],adj=1)
#text(0,-c(1:length(spcds)),"grow.",adj=c(0,0),cex=.8)
#text(0,-c(1:length(spcds)),"surv.",adj=c(0,1),cex=.8)
#text(0,-1," grow.",adj=c(0,0),cex=.8)
#text(0,-1," surv.",adj=c(0,1),cex=.8)
#mtext("N deposition",side=3,font = 2,line=-1.8)
mtext("Tree Species Decrease in Growth and Survival with N deposition",side=2,font=2,line=0)
for(i in 0:6) lines(rep(i*5,2),c(-0,-length(spcds)-1),col="gray",lwd=.5)
for(i in 0:floor(length(spcds)/3)) lines(c(0,40),rep(-i*3-.5,2),col="gray",lwd=.5)
axis(1,at=0:6*5,pos = -length(spcds)-1)
axis(1,at=0:70,labels = NA,tck=-.01,pos = -length(spcds)-1)
text(15,-length(spcds)-4,expression("N deposition (kg N ha"^-1*"yr"^-1*")"),adj=.5,xpd=NA)

#### N_CL polygon ####
# survival
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min_N,max_N,max_N,min_N)],c(-rep(i+.4,2),-i,-i),col="gray",border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min(Npmr_0.9,max_N),(max_N),(max_N),min(Npmr_0.9,max_N))],c(-rep(i+.4,2),-i,-i),col=gb(5)[5],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min(Npmr_0.95,max_N),min(Npmr_0.9,max_N),min(Npmr_0.9,max_N),min(Npmr_0.95,max_N))],c(-rep(i+.4,2),-i,-i),col=gb(5)[4],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min(Npmr_0.98,max_N),min(Npmr_0.95,max_N),min(Npmr_0.95,max_N),min(Npmr_0.98,max_N))],c(-rep(i+.4,2),-i,-i),col=gb(5)[3],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min(Npmr_0.99,max_N),min(Npmr_0.98,max_N),min(Npmr_0.98,max_N),min(Npmr_0.99,max_N))],c(-rep(i+.4,2),-i,-i),col=gb(5)[2],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min(Npmr_1,max_N),min(Npmr_0.99,max_N),min(Npmr_0.99,max_N),min(Npmr_1,max_N))],c(-rep(i+.4,2),-i,-i),col=gb(5)[1],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="survival"][match(spcds[i],spcd),c(min_N,min(Npmr_1,max_N),min(Npmr_1,max_N),min_N)],c(-rep(i+.4,2),-i,-i),col="gray",border=NA)

# growth
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min_N,max_N,max_N,min_N)],c(-rep(i-.4,2),-i,-i),col="gray",border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min(Npmr_0.9,max_N),(max_N),(max_N),min(Npmr_0.9,max_N))],c(-rep(i-.4,2),-i,-i),col=rb(5)[5],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min(Npmr_0.95,max_N),min(Npmr_0.9,max_N),min(Npmr_0.9,max_N),min(Npmr_0.95,max_N))],c(-rep(i-.4,2),-i,-i),col=rb(5)[4],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min(Npmr_0.98,max_N),min(Npmr_0.95,max_N),min(Npmr_0.95,max_N),min(Npmr_0.98,max_N))],c(-rep(i-.4,2),-i,-i),col=rb(5)[3],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min(Npmr_0.99,max_N),min(Npmr_0.98,max_N),min(Npmr_0.98,max_N),min(Npmr_0.99,max_N))],c(-rep(i-.4,2),-i,-i),col=rb(5)[2],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min(Npmr_1,max_N),min(Npmr_0.99,max_N),min(Npmr_0.99,max_N),min(Npmr_1,max_N))],c(-rep(i-.4,2),-i,-i),col=rb(5)[1],border=NA)
for(i in 1:length(spcds)) polygon(spcds.N[response=="growth"][match(spcds[i],spcd),c(min_N,min(Npmr_1,max_N),min(Npmr_1,max_N),min_N)],c(-rep(i-.4,2),-i,-i),col="gray",border=NA)

# ecoregion N dep limits
#lines(rep(min(eco.Ndep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="blue")
#lines(c(min(eco.Ndep.val,na.rm=T),min(eco.Ndep.val,na.rm=T)-1),c(-0.3,0),col="blue")
#text(min(eco.Ndep.val,na.rm=T)-1.1,0,round(min(eco.Ndep.val,na.rm=T),1),adj=1,col="blue")
#lines(rep(max(eco.Ndep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="blue")
#lines(c(max(eco.Ndep.val,na.rm=T),max(eco.Ndep.val,na.rm=T)+1),c(-0.3,0),col="blue")
#text(max(eco.Ndep.val,na.rm=T)+1.1,0,round(max(nps.Ndep.val,na.rm=T),1),adj=0,col="blue")
# park N dep limits
lines(rep(min(nps.Ndep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="darkgreen")
lines(c(min(nps.Ndep.val,na.rm=T),min(nps.Ndep.val,na.rm=T)-1),c(-0.3,1),col="darkgreen")
text(min(nps.Ndep.val,na.rm=T)-1.1,1,round(min(nps.Ndep.val,na.rm=T),1),adj=1,col="darkgreen")
lines(rep(max(nps.Ndep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="darkgreen")
lines(c(max(nps.Ndep.val,na.rm=T),max(nps.Ndep.val,na.rm=T)+1),c(-0.3,1),col="darkgreen")
text(max(nps.Ndep.val,na.rm=T)+1.1,1,round(max(nps.Ndep.val,na.rm=T),1),adj=0,col="darkgreen")


#### S_CL ####
SNtb<-SNt[NS_.best.model==1|S_.best.model==1]
spcds.S<-SNtb[spcd%in%cls$spcd,min(Spmr_1,na.rm=T),by=c("spcd","response","max_S","min_S")][order(-V1)]
if(nrow(spcds.S)==0){
  spcds.S<-cls[,c("spcd","GENUS","SPECIES")]
  spcds.S[,(c("response","max_S","min_S","V1")):=NA]
} 
spcds.S[,(grep("Spmr_|Spmr..si",names(SNtb),value = T)):=SNtb[match(spcds.S[,paste(spcd,response,V1)],paste(spcd,response,Spmr_1)),grep("Spmr_|Spmr..si",names(SNtb),value = T),with=F]]
spcds<-spcds.S[,min(Spmr_1,na.rm=T),by="spcd"][order(-V1),spcd]
spcds<-spcds[spcds%in%park.veg.sub$spcd]
#spcds.S[,grep("Spmr_|Spmr..si",names(SNtb),value = T),with=F]>spcds.S[,max_S]
par(fig=c(.5,1,0,1),mar=c(3,0,0,2),oma=c(0,3,6,1),new=T)
plot(x = c(-5,max(30,max(eco.Sdep.val)*1.1,na.rm=T)),y = c(1,-max(57,length(spcds))),axes=F,ylab="",xlab="",type="n")
text(-.2,-c(1:length(spcds)),spcd.ref[match(spcds,SPCD),SPECIES_SYMBOL],adj=1)
#text(0,-1," grow.",adj=c(0,0),cex=.8)
#text(0,-1," surv.",adj=c(0,1),cex=.8)
#mtext("S deposition",side=3,font = 2,line=-1.8)
mtext("Tree Species Decrease in Growth and Survival with S deposition",side=2,font=2,line=0)
for(i in 0:6) lines(rep(i*5,2),c(-0,-length(spcds)-1),col="gray",lwd=.5)
for(i in 0:floor(length(spcds)/3)) lines(c(0,40),rep(-i*3-.5,2),col="gray",lwd=.5)
axis(1,at=0:6*5,pos = -length(spcds)-1)
axis(1,at=0:70,labels = NA,tck=-.01,pos = -length(spcds)-1)
text(15,-length(spcds)-4,expression("S deposition (kg S ha"^-1*"yr"^-1*")"),adj=.5,xpd=NA)
#for(i in 1:length(spcds)) lines(spcds.S[response=="survival"][match(spcds[i],spcd),c(min_S,max_S)],-rep(i,2),lty=3)
#for(i in 1:length(spcds)) points(spcds.S[response=="survival"][match(spcds[i],spcd),c(min_S,max_S)],-rep(i,2),lty=3,pch=20,cex=.3)
#### S_CL polygon ####
# survival
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min_S,max_S,max_S,min_S)],c(-rep(i+.4,2),-i,-i),col="gray",border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min(Spmr_0.9,max_S),(max_S),(max_S),min(Spmr_0.9,max_S))],c(-rep(i+.4,2),-i,-i),col=gb(5)[5],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min(Spmr_0.95,max_S),min(Spmr_0.9,max_S),min(Spmr_0.9,max_S),min(Spmr_0.95,max_S))],c(-rep(i+.4,2),-i,-i),col=gb(5)[4],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min(Spmr_0.98,max_S),min(Spmr_0.95,max_S),min(Spmr_0.95,max_S),min(Spmr_0.98,max_S))],c(-rep(i+.4,2),-i,-i),col=gb(5)[3],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min(Spmr_0.99,max_S),min(Spmr_0.98,max_S),min(Spmr_0.98,max_S),min(Spmr_0.99,max_S))],c(-rep(i+.4,2),-i,-i),col=gb(5)[2],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min(Spmr_1,max_S),min(Spmr_0.99,max_S),min(Spmr_0.99,max_S),min(Spmr_1,max_S))],c(-rep(i+.4,2),-i,-i),col=gb(5)[1],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="survival"][match(spcds[i],spcd),c(min_S,min(Spmr_1,max_S),min(Spmr_1,max_S),min_S)],c(-rep(i+.4,2),-i,-i),col="gray",border=NA)

# growth
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min_S,max_S,max_S,min_S)],c(-rep(i-.4,2),-i,-i),col="gray",border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min(Spmr_0.9,max_S),(max_S),(max_S),min(Spmr_0.9,max_S))],c(-rep(i-.4,2),-i,-i),col=rb(5)[5],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min(Spmr_0.95,max_S),min(Spmr_0.9,max_S),min(Spmr_0.9,max_S),min(Spmr_0.95,max_S))],c(-rep(i-.4,2),-i,-i),col=rb(5)[4],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min(Spmr_0.98,max_S),min(Spmr_0.95,max_S),min(Spmr_0.95,max_S),min(Spmr_0.98,max_S))],c(-rep(i-.4,2),-i,-i),col=rb(5)[3],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min(Spmr_0.99,max_S),min(Spmr_0.98,max_S),min(Spmr_0.98,max_S),min(Spmr_0.99,max_S))],c(-rep(i-.4,2),-i,-i),col=rb(5)[2],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min(Spmr_1,max_S),min(Spmr_0.99,max_S),min(Spmr_0.99,max_S),min(Spmr_1,max_S))],c(-rep(i-.4,2),-i,-i),col=rb(5)[1],border=NA)
for(i in 1:length(spcds)) polygon(spcds.S[response=="growth"][match(spcds[i],spcd),c(min_S,min(Spmr_1,max_S),min(Spmr_1,max_S),min_S)],c(-rep(i-.4,2),-i,-i),col="gray",border=NA)

# ecoregion S dep limits
#lines(rep(min(eco.Sdep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="blue")
#lines(c(min(eco.Sdep.val,na.rm=T),min(eco.Sdep.val,na.rm=T)-1),c(-0.3,0),col="blue")
#text(min(eco.Sdep.val,na.rm=T)-1.1,0,round(min(eco.Sdep.val,na.rm=T),1),adj=1,col="blue")
#lines(rep(max(eco.Sdep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="blue")
#lines(c(max(eco.Sdep.val,na.rm=T),max(eco.Sdep.val,na.rm=T)+1),c(-0.3,0),col="blue")
#text(max(eco.Sdep.val,na.rm=T)+1.1,0,round(max(eco.Sdep.val,na.rm=T),1),adj=0,col="blue")
# park S dep limits
lines(rep(min(nps.Sdep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="darkgreen")
lines(c(min(nps.Sdep.val,na.rm=T),min(nps.Sdep.val,na.rm=T)-1),c(-0.3,1),col="darkgreen")
text(min(nps.Sdep.val,na.rm=T)-1.1,1,round(min(nps.Sdep.val,na.rm=T),1),adj=1,col="darkgreen")
lines(rep(max(nps.Sdep.val,na.rm=T),2),c(-0.5,-length(spcds)-1),col="darkgreen")
lines(c(max(nps.Sdep.val,na.rm=T),max(nps.Sdep.val,na.rm=T)+1),c(-0.3,1),col="darkgreen")
text(max(nps.Sdep.val,na.rm=T)+1.1,1,round(max(nps.Sdep.val,na.rm=T),1),adj=0,col="darkgreen")


mtext(c(paste("NPS unit:", paste(unique(nps[grep(park,nps$UNIT_CODE),]$PNAME),collapse = " & ")),paste("Ecoregion level III:",paste(unique(eco4[eco4$US_L4CODE%in%t.eco,]$US_L3NAME),collapse = "-"))),outer = T,font = 2,line=c(4,3),cex=1.2,at=0,adj=0) # ecoregion name
N.range<-paste("Park N deposition range:",round(min(nps.Ndep.val,na.rm = T),1),"-",round(max(nps.Ndep.val,na.rm = T),1))
S.range<-paste("Park S deposition range:",round(min(nps.Sdep.val,na.rm = T),1),"-",round(max(nps.Sdep.val,na.rm = T),1))
mtext(bquote(paste(.(N.range)~"kg N ha"^-1*"yr"^-1*"")),line=1.75,at = .05,adj = 0,outer=T)
mtext(bquote(paste(.(S.range)~"kg S ha"^-1*"yr"^-1*"")),line=.75,at = .05,adj=0,outer=T)
mtext(paste("Critical loads for ",nrow(cls[g.CL.N!="!"&spcd%in%unique(c(g.tre.sub$SPCD,s.tre.sub$SPCD))])," of ",length(unique(c(g.tre.sub$SPCD,s.tre.sub$SPCD)))," species in ecoregion FIA plots estimated", collapse = ""),line=.1,at=.05,adj=0,outer = T)# _ species of _ species determined for ecoregion
mtext(paste("Critical loads for ",length(unique(park.veg.sub[in.ft2==1,paste(GENUS,SPECIES)]))," of ",length(unique(park.veg.sub[,paste(GENUS,SPECIES)]))," species in park inventory estimated", collapse = ""),line=-.9,at=.05,adj=0,outer = T)# _ species of _ species determined for park

par(fig=c(0,1,0,1),mar=c(0,0,0,0),oma=c(0,0,0,0),new=T)
plot(0,0,type="n",axes=F,xlab="",ylab="")
text(0,0,"DRAFT",adj=c(.5,.5),cex=20,col=rgb(0,0,0,.2,maxColorValue = 1),srt=60)

#### CL barplot legend ####
par(oma=c(1,1,1,1),mar=c(0,0,0,0),new=T)
plot(c(0,1),c(0,1),axes=F,bty="n",type="n",xlab="",ylab="")
legend(.62,1.035,legend = c("0-1%","1-2%","2-5%","5-10%",">10%","no decrease", "park dep. range", "eco. dep. range"),ncol = 2,fill=c((rb(5)),"gray",NA,NA),col=c(rep(NA,6),"darkgreen","blue"),border = NA,pch = c(rep(NA,8)), lty=c(rep(NA,6),1,1),title = "decrease in growth or survival (%)",text.col = "white",bty='n')
legend(.635,1.035,legend = c("0-1%","1-2%","2-5%","5-10%",">10%","no decrease", "park dep. range", "eco. dep. range"),ncol = 2,fill=c((gb(5)),"gray",NA,NA),col=c(rep(NA,6),"darkgreen","blue"),border = NA,pch = c(rep(NA,8)), lty=c(rep(NA,6),1,1),title = "decrease in growth or survival (%)",bty = 'n')

} # end of tn loop - species list pages



Kevin Horn - Ph.D.
Department of Forest Resources and Environmental Conservation
Cheatham Hall (0324), Room 313
310 West Campus Dr.
Blacksburg, VA 24061
email: kjhorn@vt.edu


On Thu, Nov 29, 2018 at 12:13 PM Bell, Michael <michael_d_bell@nps.gov> wrote:
  Thanks,

I'm still tracking down the R code for the alt-text. It turns out what I thought had been completed was not, so we are looking for a different source.

-Mike


