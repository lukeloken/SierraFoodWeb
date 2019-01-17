#Code to read in isotope data


library(readxl)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Box folder where data and outputs are stored
box_dir<-'C:/Users/lcloken/Box/SadroLab/SierraFoodWeb'

files<-list.files(paste(box_dir,"Data", sep='/'))

CompiledDataAll<-read_excel_allsheets(paste(box_dir,"Data", "Compiled Isotope Data_v6.xlsx", sep='/'))

summary(CompiledDataAll)

LakeSummary<-CompiledDataAll$`Lake summary data`
IsotopeAll<-CompiledDataAll$`Compiled isotope data solids`



plot(IsotopeAll$Elevation, IsotopeAll$"δ13C ‰ vs VPD")

IsotopeSub<-IsotopeAll[IsotopeAll$`Sample type`=='Zooplankton',]



IsotopeSub$Group<-factor(IsotopeSub$`Sample type`)
IsotopeSub$Depth<-IsotopeSub$`Max Depth`
IsotopeSub$delC<-IsotopeSub$"δ13C ‰ vs VPD"

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
# colors<-color.palette(length(unique(merge_df$Station)))


ggplot(IsotopeSub, aes_string("Elevation", "delC", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(IsotopeSub$Group)))) + 
  scale_colour_manual(values = color.palette(length(unique(IsotopeSub$Group)))) +
  geom_point(size=2, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')
IsotopeSub$`delC`
