#Code to read in isotope data


library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Box folder where data and outputs are stored
box_dir<-'C:/Users/lcloken/Box/SadroLab/SierraFoodWeb'

files<-list.files(paste(box_dir,"Data", sep='/'))

CompiledDataAll<-read_excel_allsheets(paste(box_dir,"Data", "Compiled Isotope Data_v6.xlsx", sep='/'))

summary(CompiledDataAll)

LakeSummary<-CompiledDataAll$`Lake summary data`
IsotopeAll<-CompiledDataAll$`Compiled isotope data solids`

IsotopeAll$Elevation<-LakeSummary$Elevation[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]

IsotopeAll$CO2Sat<-LakeSummary$"CO2 %sat"[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]
IsotopeAll$CO2uM<-LakeSummary$"CO2 uM"[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]
IsotopeAll$pH_field<-LakeSummary$`pH Field`[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]
IsotopeAll$pH_lab<-LakeSummary$`pH lab`[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]
IsotopeAll$delH_water<-LakeSummary$`d2H water SMOW`[match(IsotopeAll$`Lake`, LakeSummary$`Lake Name`)]

LakeSummary$delH_water<-LakeSummary$`d2H water SMOW`
LakeSummary$Group2<-'Water'

plot(IsotopeAll$CO2Sat, IsotopeAll$Elevation)
plot(IsotopeAll$CO2uM, IsotopeAll$Elevation)


IsotopeSub<-IsotopeAll[IsotopeAll$`Sample type` %in% c('Zooplankton', 'DIC', 'Sediment abyssal', 'POM', 'DOC', 'Terrestrial veg', 'Fish', 'SAP', 'EAP', 'Aquatic Insect', 'Benthic Infauna'),]



IsotopeSub$Group<-factor(IsotopeSub$`Sample type`, c('Fish',  'Zooplankton', 'Aquatic Insect', 'Benthic Infauna','Sediment abyssal', 'POM', 'DOC', 'Terrestrial veg',  'SAP', 'EAP', 'DIC'))
IsotopeSub$Depth<-IsotopeSub$`Max Depth`
IsotopeSub$delC<-IsotopeSub$"δ13C ‰ vs VPD"
IsotopeSub$delN<-IsotopeSub$"δ15N ‰ vs Air-N2"
IsotopeSub$delH<-IsotopeSub$"δ2H\r\n(per mil vs. VSMOW)"

IsotopeSub$delH_discrim<-(IsotopeSub$delH- IsotopeSub$delH_water)/(1+IsotopeSub$delH_water/1000)

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colorset<-'Paired'
colors<-c(brewer.pal(length(unique(IsotopeSub$Group))-1, colorset), 'black')


png(paste0(box_dir, '/Figures/delC_delH.png'), units='in', width=7, height=7, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH),], aes(x=delC, y=delH, group=Group)) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()



# colors<-color.palette(length(unique(merge_df$Station)))

png(paste0(box_dir, '/Figures/delC_Elevation.png'), units='in', width=7, height=7, res=400, bg='white')


ggplot(IsotopeSub, aes_string("Elevation", "delC", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=40, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()

#Nitrogen
png(paste0(box_dir, '/Figures/delN_Elevation.png'), units='in', width=7, height=7, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delN),], aes_string("Elevation", "delN", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=40, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()

#Hydrogen
png(paste0(box_dir, '/Figures/delH_Elevation.png'), units='in', width=7, height=7, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH),], aes_string("Elevation", "delH", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(color=Group, fill=Group)) +
  geom_jitter(size=2, width=40, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()

#Hydrogen discrimination factor
png(paste0(box_dir, '/Figures/delH_disrim_Elevation.png'), units='in', width=7, height=7, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH_discrim),], aes_string("Elevation", "delH_discrim", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(color=Group, fill=Group)) +
  geom_jitter(size=2, width=40, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()



#Vs CO2 concentration



png(paste0(box_dir, '/Figures/delC_vCO2facet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub, aes_string("CO2Sat", "delC", group="Group")) + 
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

dev.off()



png(paste0(box_dir, '/Figures/delC_vpHfacet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub, aes_string("pH_field", "delC", group="Group")) + 
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

dev.off()


png(paste0(box_dir, '/Figures/delC_vCO2.png'), units='in', width=5, height=5, res=400, bg='white')

ggplot(IsotopeSub, aes_string("CO2Sat", "delC", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=40, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

dev.off()




png(paste0(box_dir, '/Figures/delC_vElevationfacet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub, aes_string("Elevation", "delC", group="Group")) + 
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

dev.off()



png(paste0(box_dir, '/Figures/delH_vElevationfacet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH),], aes_string("Elevation", "delH", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(-300,-100)) + 
  # geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black') 
  facet_wrap(Group ~ .)

dev.off()


png(paste0(box_dir, '/Figures/delH_vElevationfacet_withH2O.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH),], aes_string("Elevation", "delH", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(-290,-35)) + 
  geom_point(data=LakeSummary, aes_string(x='Elevation', y="delH_water", group="Group2"), color='darkgrey', size=1) +
  geom_smooth(data=LakeSummary, aes_string(x='Elevation', y="delH_water", group='Group2'), color='darkgrey', size=1, method='lm', se=F, linetype=2) +
# geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black')
  facet_wrap(Group ~ .)

dev.off()


png(paste0(box_dir, '/Figures/delHdiscrim_vElevationfacet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH_discrim),], aes_string("Elevation", "delH_discrim", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(-220,-100)) + 
  # geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black') 
  facet_wrap(Group ~ .)

dev.off()


png(paste0(box_dir, '/Figures/delHdiscrim_boxplot_byGroup.png'), units='in', width=8, height=4, res=400, bg='white')

ggplot(IsotopeSub[is.finite(IsotopeSub$delH_discrim),], aes_string("Group", "delH_discrim")) + 
  labs(y=expression(paste(Delta, 'H (', "\u2030", ")"))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  # geom_hline(yintercept = c(-79, -173, -85), colour=colors[c(8, 6, 7)], linetype=2, size=1) + 
  geom_hline(yintercept = c(-79, -173, -85), colour=colors[c(8, 6, 7)], linetype=2, size=1) + 
  geom_ribbon(x=c(0:(length(IsotopeSub[is.finite(IsotopeSub$delH_discrim),]$Group)-1)), ymin=(-79-16), ymax = (-79+16) , fill=colors[8], alpha=0.2) + 
  geom_ribbon(x=c(0:(length(IsotopeSub[is.finite(IsotopeSub$delH_discrim),]$Group)-1)), ymin=(-173-26), ymax = (-173+26) , fill=colors[6], alpha=0.2) + 
  geom_ribbon(x=c(0:(length(IsotopeSub[is.finite(IsotopeSub$delH_discrim),]$Group)-1)), ymin=(-85-19), ymax = (-85+19) , fill=colors[7], alpha=0.2) + 
  geom_jitter(size=1, width=.1, aes(colour=Group)) + 
  geom_boxplot(alpha=0.2, aes(fill=Group), outlier.shape=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom',legend.title=element_blank(), axis.text.x = element_text(size = 6), axis.title.x=element_blank()) +
  scale_y_continuous(limits=c(-225,-35)) 

  # geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black') 
  # facet_wrap(Group ~ .)

dev.off()


png(paste0(box_dir, '/Figures/delH_vCO2Satfacet.png'), units='in', width=10, height=10, res=400, bg='white')


ggplot(IsotopeSub[is.finite(IsotopeSub$delH),], aes_string("CO2Sat", "delH", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_smooth(method='lm', alpha=0.2, se=F, aes(fill=Group, colour=Group)) +
  geom_jitter(size=2, width=0, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(-300,-100)) + 
  # geom_point(size=2, width=40, aes_string(data=IsotopeSub[IsotopeSub$Group=='DIC',] , x="CO2Sat", y="delC"), fill='black', col='black') 
  facet_wrap(Group ~ .)

dev.off()


png(paste0(box_dir, '/Figures/delH_Water_VCO2andElevation.png'), units='in', width=12, height=4, res=400, bg='white')

par(mfrow=c(1,3), pch=16)
par(mar=c(3,2,.5,.5), mgp=c(2,.5,0), oma=c(0,1,0,0), tck=-.02)

plot(LakeSummary$`CO2 uM`, LakeSummary$`d2H water SMOW`, xlab=expression(paste(CO[2], ' (', mu, 'M)', sep='')), ylab='')
mtext(expression(paste(delta, ''^'2', 'H-', H[2], 'O (', "\u2030", ')', sep='')), 2, 1.5)

plot(LakeSummary$Elevation, LakeSummary$`d2H water SMOW`, xlab='Elevation (m)', ylab='')

plot(LakeSummary$`Max Depth`, LakeSummary$`d2H water SMOW`, xlab='Depth (m)', ylab='')


dev.off()

