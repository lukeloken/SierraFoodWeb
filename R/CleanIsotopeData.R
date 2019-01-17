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


IsotopeSub<-IsotopeAll[IsotopeAll$`Sample type` %in% c('Zooplankton', 'DIC', 'Sediment abyssal', 'POM', 'DOC', 'Terrestrial veg'),]



IsotopeSub$Group<-factor(IsotopeSub$`Sample type`)
IsotopeSub$Depth<-IsotopeSub$`Max Depth`
IsotopeSub$delC<-IsotopeSub$"δ13C ‰ vs VPD"

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colorset<-'Dark2'
colors<-brewer.pal(length(unique(IsotopeSub$Group)), colorset)


# colors<-color.palette(length(unique(merge_df$Station)))


ggplot(IsotopeSub, aes_string("Elevation", "delC", group="Group")) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) +
  geom_jitter(size=2, width=20, aes(fill=Group, shape=Group)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

