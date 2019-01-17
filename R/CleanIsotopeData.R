#Code to read in isotope data


library(readxl)
library(plyr)
library(dplyr)
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


plot(IsotopeAll$Elevation2, IsotopeAll$"δ13C ‰ vs VPD")
points(IsotopeAll$Elevation, IsotopeAll$"δ13C ‰ vs VPD", col='red')


IsotopeSub<-IsotopeAll[IsotopeAll$`Sample type` %in% c('Zooplankton', 'DIC', 'Sediment abyssal', 'POM', 'DOC', 'Terrestrial veg', 'Fish', 'SAP', 'EAP', 'Aquatic Insect', 'Benthic Infauna'),]



IsotopeSub$Group<-factor(IsotopeSub$`Sample type`, c('Fish',  'Zooplankton', 'Aquatic Insect', 'Benthic Infauna','Sediment abyssal', 'POM', 'DOC', 'Terrestrial veg',  'SAP', 'EAP', 'DIC'))
IsotopeSub$Depth<-IsotopeSub$`Max Depth`
IsotopeSub$delC<-IsotopeSub$"δ13C ‰ vs VPD"
IsotopeSub$delN<-IsotopeSub$"δ15N ‰ vs Air-N2"
IsotopeSub$delH<-IsotopeSub$"δ2H\r\n(per mil vs. VSMOW)"


color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colorset<-'Paired'
colors<-c(brewer.pal(length(unique(IsotopeSub$Group))-1, colorset), 'black')


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
