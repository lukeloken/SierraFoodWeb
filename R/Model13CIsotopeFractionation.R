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

LakeSummary$Ep<-Ep_Smyntek(LakeSummary$`CO2 uM`)

LakeSummary$Phyto_ExpectedD13<-LakeSummary$'d13C DIC'-LakeSummary$Ep

LakeSummary$POM_ObservedD13<-IsotopeSub$"δ13C ‰ vs VPD"[match(LakeSummary$`Lake Name`, IsotopeSub$Lake)]



plot(LakeSummary$`CO2 uM`, LakeSummary$Phyto_ExpectedD13, ylim=c(-35,0), bg='red', pch=21, ylab='', xlab='')
points(LakeSummary$`CO2 uM`, LakeSummary$'d13C DIC', col='black', pch=16)
points(LakeSummary$`CO2 uM`, LakeSummary$POM_ObservedD13, bg='blue', pch=21)

arrows(x0=LakeSummary$`CO2 uM`, y0=LakeSummary$'d13C DIC', y1=LakeSummary$Phyto_ExpectedD13, length=0.1, lty=2)

legend('bottomleft', inset=0.02, c('DIC', 'Expected Phyto', 'Observed POM'), pch=c(16,21,21), col='black', pt.bg=c('black', 'red', 'blue'), bty='n')

mtext(expression(paste(CO[2], ' (', mu, 'M)', sep='')), 1, 2.5)
mtext(expression(paste(delta, ''^'13', 'C (', "\u2030", ')', sep='')), 2, 2.5)


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
