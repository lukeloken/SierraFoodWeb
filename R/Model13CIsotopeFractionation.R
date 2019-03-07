


blue.trans <- adjustcolor(colors()[62],alpha.f=0.6)
green.trans <- adjustcolor('#006837', alpha.f=0.6)
aquatic.trans <- adjustcolor('#c2e699', alpha.f=0.6)
brown.trans <- adjustcolor('brown', alpha.f=0.6)
gray.trans <- adjustcolor('gray', alpha.f=0.7)
black.trans <- adjustcolor('black',alpha.f=0.7)


# Isotope Fractionation Model

#Model Fractionaction of Daphnia galeata
CalculateEp <- function(C13Aq, C13B){
  Ep <- ((C13Aq + 1000)/(C13B + 1000)-1)*1000
  return(Ep)
}

CalculateEp(-8,-30)


CassarEp <- function(){
  
  
  del13C_Phyto<-1000*(del13C_Sample/del13C_PDB - 1)
  
}

Ep <- ( del13C_CO2 - del13C_P )/(1 + del13C_P/1000)

Wang_Ep<-function(del13C_CO2, del13C_POC){
Ep<- 1000*(del13C_CO2 - del13C_POC)/(1000 + del13C_POC)
return(Ep)
}

Wang_Ep(-8,-30)

#algal growth rate 0.23 d-1
# CI +/- 0.1 d-1
Ep_Smyntek <- function (CO2_uM){
  2.51*log(CO2_uM) + 7.87
}

Ep_Smyntek(20)

CIsotopeSummary<- IsotopeSub %>%
  select(Group, Lake, delC) %>%
  group_by(Group, Lake) %>%
  dplyr::summarize_all(mean, na.rm=T) %>%
  spread(key=Group, value=delC)
  
LakeSummary$Ep<-Ep_Smyntek(LakeSummary$`CO2 uM`)
LakeSummary$Lake = LakeSummary$'Lake Name'

LakeSummary$Phyto_ExpectedD13<-LakeSummary$Money-LakeSummary$Ep

LakeSummary2<-full_join(LakeSummary, CIsotopeSummary)

LakeSummary2$PhytoExpected_Epi<-LakeSummary2$Money-LakeSummary2$Ep

# LakeSummary$Sediment_ObservedD13<-IsotopeSub$"δ13C ‰ vs VPD"[match(LakeSummary$`Lake Name`, IsotopeSub$Lake)]


yvars <- c('del13C')
xvar <- 'CO2 uM'

ylims <- c(-38, 5)
xlims <- c(0,50)
xaxint <- 400

LakeSummary2$CO2uM<-LakeSummary2$`CO2 uM`


co2_x= data.frame(CO2uM=seq(-10, 50,1))

Ter_lm<-lm(`Terrestrial veg` ~ CO2uM, data=LakeSummary2 )
Ter_pred<-stats::predict(Ter_lm,newdata=co2_x, interval='confidence', level=0.95)

Phyto_lm<-lm(PhytoExpected_Epi ~ CO2uM, data=LakeSummary2 )
Phyto_pred<-stats::predict(Phyto_lm,newdata=co2_x, interval='confidence', level=0.95)

POM_lm<-lm(POM ~ CO2uM, data=LakeSummary2 )
POM_pred<-stats::predict(POM_lm,newdata=co2_x, interval='confidence', level=0.95)

DIC_lm<-lm(`d13C DIC` ~ CO2uM, data=LakeSummary2 )
DIC_pred<-stats::predict(DIC_lm,newdata=co2_x, interval='confidence', level=0.95)

Money_lm<-lm(Money ~ CO2uM, data=LakeSummary2 )
Money_pred<-stats::predict(Money_lm,newdata=co2_x, interval='confidence', level=0.95)


terr_veg <- c('Tree_Pinaceae', 'Tree non-pine', 'Grass','Shrub','Soil')#sample types lumped together as terr veg
aq_veg <- c('SAP','EAP','Wetland plants')

terrveg.sd<-sd(LakeSummary2$`Terrestrial veg`,na.rm=TRUE)
pom.sd<-sd(LakeSummary2$POM,na.rm=TRUE)
dic.sd<-sd(LakeSummary2$DIC,na.rm=TRUE)
money.sd<-sd(LakeSummary2$Money,na.rm=TRUE)



png(paste0(box_dir, '/Figures/FoodWebPaper_Figure3_d13C.png'), units='in', width=5.5, height=10, res=300, bg='white')

par(mfrow=c(3,1),mar=c(3.5,3,1,0.5),oma=c(2,2,1,1), xpd=F)

#plot 1

plot(LakeSummary2$Elevation, LakeSummary2$`CO2 uM`, ylab='', xlab='', las=1, col='black', cex=1.4, axes=F,pch=16)
abline(lm( LakeSummary2$`CO2 uM` ~ LakeSummary2$Elevation ), lwd=2)

axis(side=2,lwd=2,las=2, cex.axis=1.2,col.axis='black')
box(lwd=2)
mtext(side=1, line=2.7, expression(paste("Elevation (m)")), cex=1.2)
axis(side=1, lwd=2, cex.axis=1.2,col.axis='black')
mtext(side=2, line=2.9, expression(paste(CO[2], " (", mu, "M)")), cex=1.2)

#plot 2

plot(LakeSummary2$`CO2 uM`, LakeSummary2$Ep, ylab='', xlab='', las=1, col='black', type='p', cex=1.4, axes=F, pch=16)
lines(seq(0.01, 50, 0.1), Ep_Smyntek(seq(0.01, 50, 0.1)))


axis(side=2,lwd=2,las=2, cex.axis=1.2,col.axis='black')
box(lwd=2)
mtext(side=2, line=2.9, expression(paste(epsilon[p], ' (', "\u2030", ')', sep='')), cex=1.2)
axis(side=1, lwd=2, cex.axis=1.2,col.axis='black')
mtext(side=1, line=2.7, expression(paste(CO[2], " (", mu, "M)")), cex=1.2)


#plot 3

plot(LakeSummary2$`CO2 uM`, LakeSummary2$'d13C DIC', ylim=c(-38,-5), ylab='', xlab='', las=1, col=blue.trans, type='n', cex=1.4, axes=F)

polygon(x=c(co2_x[,1],rev(co2_x[,1])),y=c((Ter_pred[,1]-terrveg.sd), rev((Ter_pred[,1]+terrveg.sd))),col=green.trans,border=NA)
# polygon(x=c(co2_x[,1],rev(co2_x[,1])),y=c((DIC_pred[,1]-dic.sd), rev((DIC_pred[,1]+dic.sd))),col=gray.trans,border=NA)
polygon(x=c(co2_x[,1],rev(co2_x[,1])),y=c((Money_pred[,1]-money.sd), rev((Money_pred[,1]+money.sd))),col=gray.trans,border=NA)
polygon(x=c(co2_x[,1],rev(co2_x[,1])),y=c((Phyto_pred[,1]-2), rev((Phyto_pred[,1]+3))),col=aquatic.trans,border=NA)


abline(lm( LakeSummary2$'Terrestrial veg' ~ LakeSummary2$`CO2 uM` ), col=green.trans, lwd=2)
# abline(lm( LakeSummary2$'d13C DIC' ~ LakeSummary2$`CO2 uM` ), col=blue.trans, lwd=2)
abline(lm( LakeSummary2$Money ~ LakeSummary2$`CO2 uM` ), col=gray.trans, lwd=2)
abline(lm( LakeSummary2$PhytoExpected_Epi ~ LakeSummary2$`CO2 uM` ), col=aquatic.trans, lwd=2)
abline(lm( LakeSummary2$POM ~ LakeSummary2$`CO2 uM` ), col="black", lwd=2)

x_arrow<-50
arrows(x0=co2_x[x_arrow,1], y0=Money_pred[x_arrow,1], y1=Phyto_pred[x_arrow,1], length=0.1, lty=1, col='black')
text(co2_x[x_arrow,1], mean(c(Money_pred[x_arrow,1], Phyto_pred[x_arrow,1])), expression(paste(epsilon[p])), pos=4, cex=2)
# points(LakeSummary2$`CO2 uM`, LakeSummary2$DOC, bg='orange', pch=22)
# points(LakeSummary2$`CO2 uM`, LakeSummary2$`Sediment abyssal`, bg='brown', pch=23)
# points(LakeSummary2$`CO2 uM`, LakeSummary2$Zooplankton, bg='purple', pch=24)
# points(LakeSummary2$`CO2 uM`, LakeSummary2$'d13C DIC', col=blue.trans, pch=16, cex=1.4)

points(LakeSummary2$`CO2 uM`, LakeSummary2$Money, col=gray.trans, pch=16, cex=1.4)

points(LakeSummary2$`CO2 uM`, LakeSummary2$PhytoExpected_Epi, pch=21, cex=1.4, col=green.trans)
points(LakeSummary2$`CO2 uM`, LakeSummary2$'Terrestrial veg', pch=16, cex=1.4, col=green.trans)
points(LakeSummary2$`CO2 uM`, LakeSummary2$POM, col='black', pch=16, cex=1.4)


legend('topleft', legend=c('CO2','Terr Veg','Phyto','POM'),pch=21,col=c(gray.trans,green.trans,green.trans,black.trans),pt.bg=c(gray.trans,green.trans,aquatic.trans,black.trans), cex=1.1 ,horiz=T)


# legend('topleft', inset=0.02, c('Epilimnion DIC', 'Expected Phyto', 'Observed POM'), pch=c(2,8,21), col=c('black', 'darkgreen', 'black'), pt.bg=c('green1'), bty='n', ncol=2)
# 

axis(side=2,lwd=2,las=2, cex.axis=1.2,col.axis='black')
box(lwd=2)
mtext(side=2, line=2.9, expression(paste(delta^{13}, "C Resource (\u2030)")), cex=1.2)
axis(side=1, lwd=2, cex.axis=1.2,col.axis='black')
mtext(side=1, line=2.7, expression(paste(CO[2], " (", mu, "M)")), cex=1.2)


dev.off()




# plot(LakeSummary2$`CO2 uM`, LakeSummary2$'d13C DIC', col='black', pch=2, ylim=c(-38,5), ylab='', xlab='', las=1, type='n')

plot(IsotopeSub[IsotopeSub$'Sample description' %in% terr_veg,xvar],IsotopeSub[IsotopeSub$'Sample description' %in% terr_veg,yvars[1]], pch=NA,xlim=xlims,ylim=ylims, 
     col=green.trans,bg=green.trans,cex=1.3,yaxs='i',xaxs='i', axes=F,ann=F)
polygon(x=c(pred.phyto$Elevation,rev(pred.phyto$Elevation)), y=c(veg_elev_preds[,1]-terrveg.sd,rev(veg_elev_preds[,1]+terrveg.sd)),col=green.trans,border=NA)
abline(veg_elev.lm,lwd=2,col=green.trans)
#Predicted phyto signature (subtract Eh from d2H-H2O, plus or minus 1 sd for EH)
polygon(x=c(pred.phyto$Elevation,rev(pred.phyto$Elevation)), y=c(pred.d2H[,1]-177, rev(pred.d2H[,1])-123),col=aquatic.trans,border=NA)
abline( predphyto.lm, lwd=2, col=aquatic.trans)
abline(pom_elev.lm,lwd=2,col=black.trans)

axis(1)
axis(2)

mtext(expression(paste(CO[2], ' (', mu, 'M)', sep='')), 1, 1.5)
mtext(expression(paste(delta, ''^'13', 'C (', "\u2030", ')', sep='')), 2, 1.5)


dev.off()





png(paste0(box_dir, '/Figures/Expected_delC_Phyto_withgroups.png'), units='in', width=5, height=4, res=400, bg='white')
par(mar=c(3,3,.5,.5), mgp=c(2,.3,0), tck=-.01)
plot(LakeSummary2$`CO2 uM`, LakeSummary2$'d13C DIC', col='black', pch=2, ylim=c(-38,5), ylab='', xlab='', las=1)
arrows(x0=LakeSummary2$`CO2 uM`, y0=LakeSummary2$'d13C DIC', y1=LakeSummary2$PhytoExpected_Epi, length=0.1, lty=1, col='grey')

points(LakeSummary2$`CO2 uM`, LakeSummary2$DOC, bg='orange', pch=22)
points(LakeSummary2$`CO2 uM`, LakeSummary2$`Sediment abyssal`, bg='brown', pch=23)
points(LakeSummary2$`CO2 uM`, LakeSummary2$Zooplankton, bg='purple', pch=24)
points(LakeSummary2$`CO2 uM`, LakeSummary2$PhytoExpected_Epi, pch=8, col='darkgreen')
points(LakeSummary2$`CO2 uM`, LakeSummary2$POM, bg='green3', pch=21)

legend('topleft', inset=0.02, c('Epilimnion DIC', 'Expected Phyto', 'Observed POM', 'Observed DOM', 'Sediments', 'Zoops'), pch=c(2,8,21, 22, 23, 24), col='black', pt.bg=c('black', 'green1', 'darkgreen', 'orange', 'brown', 'purple'), bty='n', ncol=3)

mtext(expression(paste(CO[2], ' (', mu, 'M)', sep='')), 1, 1.5)
mtext(expression(paste(delta, ''^'13', 'C (', "\u2030", ')', sep='')), 2, 1.5)

dev.off()


png(paste0(box_dir, '/Figures/FractionactionVCO2_Scatter.png'), units='in', width=5, height=4, res=400, bg='white')
par(mar=c(3,3,.5,.5), mgp=c(2,.3,0), tck=-.01)

plot(LakeSummary2$`CO2 uM`, LakeSummary2$Ep, col='black', pch=16, ylab='', xlab='', las=1)

mtext(expression(paste(CO[2], ' (', mu, 'M)', sep='')), 1, 1.5)
mtext(expression(paste(epsilon[p], ' (', "\u2030", ')', sep='')), 2, 1.5)

dev.off()

IsotopeSub$Ep<-Ep_Smyntek(IsotopeSub$CO2uM)

IsotopeSub$POM_Expected<-IsotopeSub$'d13C DIC'-IsotopeSub$Ep

Expected_df<-IsotopeSub[IsotopeSub$Group =='POM',]

Expected_df$delC<-Expected_df$POM_Expected

Expected_df$Group<-'Expected POM'

bound_df<-rbind(Expected_df, IsotopeSub)

# plot(IsotopeSub$CO2uM, IsotopeSub$Ep)
# 
# plot(IsotopeSub$CO2uM, IsotopeSub$delC)
# 

# png(paste0(box_dir, '/Figures/delH_vCO2Satfacet.png'), units='in', width=10, height=10, res=400, bg='white')

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colorset<-'Paired'
colors<-c(brewer.pal(length(unique(bound_df$Group))-1, colorset), 'black')


ggplot(bound_df, aes_string("CO2Sat", "delC", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') + 
  # geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black') 
  facet_wrap(Group ~ .)

ggplot(IsotopeSub[IsotopeSub$Group =='POM',], aes_string("CO2Sat", "POM_Expected", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') 

# dev.off()

plot(IsotopeSub$POM_Expected[which(IsotopeSub$Group=='POM')], IsotopeSub$delC[which(IsotopeSub$Group=='POM')])
abline(0,1)
