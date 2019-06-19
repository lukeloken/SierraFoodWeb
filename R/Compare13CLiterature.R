

# Plot temporal and vertical variability in 13C POM from literature values
# input is google drive folder of literature 13C POM or phytoplankton
# Each figure also includes data from our paper

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
library(ggplot2)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/PlotErrorBar.R')


# Box folder where data and outputs are stored
box_dir<-'C:/Users/lcloken/Box/SadroLab/SierraFoodWeb'

colorset<-'Dark2'
colors<-brewer.pal(3, colorset)[c(1,3,2)]

# Our data
IsotopePOM<-IsotopeSub[IsotopeSub$`Sample type`=='POM',]
IsotopePOM <- drop_na(IsotopePOM, delC)
IsotopePOM_deep<-dplyr::filter(IsotopePOM, Lake %in% c('Emerald', 'Oriole'))

IsotopePOM_deep$Depth_strata <- 'Epilimnion'
IsotopePOM_deep$Depth_strata[IsotopePOM_deep$`Sample ID` %in% c(419,426)] <- 'Hypolimnion'

Isotope_matchingformat <- IsotopePOM_deep %>%
  dplyr::select(Lake, Depth_strata, delC, Elevation) %>%
  rename(LakeName = Lake,
         C13mean = delC, 
         Elevation_m = Elevation) %>%
  mutate(Order = 2, 
         Depth_strata = factor(IsotopePOM_deep$Depth_strata, c('Epilimnion', "Metalimnion", 'Hypolimnion')))

boxplot(Isotope_matchingformat$C13mean~ Isotope_matchingformat$Depth_strata)


# IsotopePOM_summary<-IsotopePOM %>%
#   group_by(Lake, `Sample description`) %>%
#   summarize(meanC13 = mean(delC))



# files<-list.files(paste(box_dir,"Data", sep='/'))
# files

fig7_list<-read_excel_allsheets(paste(box_dir,"Data", "Fig7.xlsx", sep='/')) 

space_df<-fig7_list[[2]]
time_df<-fig7_list[[1]]


space_df2<-space_df %>%
  mutate(Depth_strata = factor(space_df$Depth_strata, c('Epilimnion', "Metalimnion", 'Hypolimnion'))) %>%
  select(LakeName, Depth_strata, C13mean, C13sd, Elevation_m) %>%
  drop_na(Depth_strata) %>%
  group_by(LakeName, Depth_strata) %>%
  arrange(Elevation_m, LakeName, Depth_strata) %>%
  mutate(Order = 1, 
         Elevation_m = as.numeric(Elevation_m))
  
  
space_df3<-bind_rows(space_df2, Isotope_matchingformat) %>%
  arrange(Order, Elevation_m, LakeName, Depth_strata)


name_table<-space_df3 %>%
  dplyr::select(LakeName, Depth_strata, C13mean, Order, Elevation_m) %>%
  group_by(Order, LakeName) %>%
  summarize(mean=mean(C13mean), n=n(), Elevation_m = median(Elevation_m, na.rm=T)) %>%
  dplyr::arrange(Order, Elevation_m, LakeName)

  name_table$LakeName <- gsub('Lake', '', name_table$LakeName)
  name_table$LakeName <- gsub('Reservoir', '', name_table$LakeName)
  name_table$LakeName <- gsub(' ', '', name_table$LakeName)
  
  name_table$cumulative_nu<-cumsum(name_table$n)
  name_table$axis_nu<-name_table$cumulative_nu-name_table$n/2 + .5
  
space_df3$plot_nu<-1:nrow(space_df3)
space_df3$colour_nu<-match(space_df3$Depth_strata, levels(space_df3$Depth_strata))
space_df3$colour <- colors[space_df3$colour_nu]

# ggplot(aes(x=LakeName, y=C13mean, colour=Depth_strata), data=space_df3) + 
#   # geom_boxplot(aes(colour=Depth_strata)) + 
#   geom_point(aes(colour=Depth_strata), size=2, pch=15) +
#   geom_errorbar(aes(ymin=C13mean-C13sd, ymax=C13mean+C13sd), width=.3) + 
#   theme_bw()
# 
# 

# 
# IsotopePOM_deep$plot_nu<- ((nrow(space_df2)+1):(nrow(space_df2)+nrow(IsotopePOM_deep)))
# IsotopePOM_deep$colour_nu<-match(IsotopePOM_deep$Depth_strata, levels(space_df2$Depth_strata))
# IsotopePOM_deep$colour <- colors[IsotopePOM_deep$colour_nu]
# 
# 
# boxplot(IsotopePOM_deep$delC~ IsotopePOM_deep$DepthStrata)

name_table


png(paste0(box_dir, '/Figures/Compare13CLiterature_Vertical.png'), units='in', width=6, height=3, res=300, bg='white')

par(mfrow=c(1,1),mar=c(3,3,1,0.5), xpd=F, mgp=c(1.5,0.3,0), tck=-0.02)

plot(space_df3$plot_nu, space_df3$C13mean, col=space_df3$colour, pch=15, ylim=c(-51, -23), xlim=c(0.5, max(space_df3$plot_nu)+0.5), xaxs='i', axes=F, ylab='', xlab='', cex=1)
error.bar(x=space_df3$plot_nu, y=space_df3$C13mean, upper.y=space_df3$C13sd, col=space_df3$colour, lwd=1.5, length=0.05)
abline(v=space_df3$plot_nu[which(space_df3$Depth_strata=='Epilimnion' & space_df3$Order<1.5)]-0.5, lty=3)
abline(v=max(space_df3$plot_nu[which(space_df3$Order<1.5)])+0.5)

box(which='plot')

axis(2, las=1, cex.axis=0.7)
axis(1, at=name_table$axis_nu, labels=name_table$LakeName, cex.axis=0.65)


mtext( expression(paste(delta^{13}, "C POM (\u2030)")), side=2, line=1.5)
mtext( "Lake", side=1, line=1.5)

legend('bottomright', c(levels(space_df3$Depth_strata)), text.col=colors, bty='n', cex=0.65)

dev.off()





#Time figure and analysis
#Our Data
IsotopePOM 
IsotopePOM_surf<-dplyr::filter(IsotopePOM,  `Sample ID` != 419 & `Sample ID` != 426)

Isotope_matchingformat_time <- IsotopePOM_surf %>%
  dplyr::select(Lake, delC, Elevation) %>%
  rename(LakeName = Lake,
         C13mean = delC, 
         Elevation_m = Elevation) %>%
  mutate(Order = 2)


time_df2<-time_df %>%
  select(LakeName, C13mean, C13min, C13max, Duration, Sample_Freq, Elevation_m) %>%
  drop_na(C13mean, Elevation_m) %>%
  group_by(LakeName) %>%
  arrange(Elevation_m, LakeName) %>%
  mutate(Order = 1)


time_df3<-bind_rows(time_df2, Isotope_matchingformat_time) %>%
  arrange(Order,Elevation_m, LakeName)


time_df3$LakeName <- gsub('Lake', '', time_df3$LakeName)
time_df3$LakeName <- gsub('Reservoir', '', time_df3$LakeName)
time_df3$LakeName <- gsub(' ', '', time_df3$LakeName)


time_df3$plot_nu<-1:nrow(time_df3)
# time_df3$colour_nu<-match(time_df3$Depth_strata, levels(time_df3$Depth_strata))
# time_df3$colour <- colors[time_df3$colour_nu]






png(paste0(box_dir, '/Figures/Compare13CLiterature_Temporal.png'), units='in', width=6, height=3.5, res=300, bg='white')

par(mfrow=c(1,1),mar=c(6,3,1,0.5), xpd=F, mgp=c(1.5,0.3,0), tck=-0.02)

plot(time_df3$plot_nu, time_df3$C13mean, col=colors[1], pch=15, ylim=c(-41, -12), xlim=c(0.5, max(time_df3$plot_nu)+0.5), xaxs='i', axes=F, ylab='', xlab='', cex=1)
error.bar(x=time_df3$plot_nu, y=time_df3$C13mean, upper.y=time_df3$C13max-time_df3$C13mean, lower.y=time_df3$C13mean-time_df3$C13min, lwd=1.5, length=0.03, col=colors[1])
abline(v=max(time_df3$plot_nu[which(time_df3$Order<1.5)])+0.5, lwd=1)

box(which='plot')

axis(2, las=1, cex.axis=0.7)
axis(1, at=time_df3$plot_nu, labels=time_df3$LakeName, cex.axis=0.65, las=3)

mtext( expression(paste(delta^{13}, "C POM (\u2030)")), side=2, line=1.5)
mtext( "Lake", side=1, line=3.5)

dev.off()




#Double figure


png(paste0(box_dir, '/Figures/Compare13CLiterature_TemporalAndVertical.png'), units='in', width=6, height=6, res=400, bg='white')

par(mfrow=c(2,1),mar=c(1,1,.5,0.5), oma=c(4,1.5,0,0), xpd=F, mgp=c(1.5,0.3,0), tck=-0.02)


plot(space_df3$plot_nu, space_df3$C13mean, col=space_df3$colour, pch=22, ylim=c(-51, -23), xlim=c(0.5, max(space_df3$plot_nu)+0.5), xaxs='i', axes=F, ylab='', xlab='', cex=1)
error.bar(x=space_df3$plot_nu, y=space_df3$C13mean, upper.y=space_df3$C13sd, col=space_df3$colour, lwd=1.5, length=0.05)
points(space_df3$plot_nu, space_df3$C13mean, col=space_df3$colour, pch=22, cex=1, bg='white')
points(space_df3$plot_nu[which(space_df3$Order==2)], space_df3$C13mean[which(space_df3$Order==2)], col=space_df3$colour[which(space_df3$Order==2)], pch=22, cex=1, bg=space_df3$colour[which(space_df3$Order==2)])

abline(v=space_df3$plot_nu[which(space_df3$Depth_strata=='Epilimnion' & space_df3$Order<1.5)]-0.5, lty=3)
abline(v=max(space_df3$plot_nu[which(space_df3$Order<1.5)])+0.5)

box(which='plot')

axis(2, las=1, cex.axis=0.7)
axis(1, at=name_table$axis_nu, labels=name_table$LakeName, cex.axis=0.65, mgp=c(1.5,.1, 0))


mtext( expression(paste(delta^{13}, "C POM (\u2030)")), side=2, line=0.25, outer=T)
# mtext( "Lake", side=1, line=1.5)

legend('bottomright', c(levels(space_df3$Depth_strata)), text.col=colors, bty='n', cex=0.65)


plot(time_df3$plot_nu, time_df3$C13mean, col=colors[1], pch=22, ylim=c(-41, -12), xlim=c(0.5, max(time_df3$plot_nu)+0.5), xaxs='i', axes=F, ylab='', xlab='', cex=1)
error.bar(x=time_df3$plot_nu, y=time_df3$C13mean, upper.y=time_df3$C13max-time_df3$C13mean, lower.y=time_df3$C13mean-time_df3$C13min, col=colors[1], lwd=1.5, length=0.03)
points(time_df3$plot_nu, time_df3$C13mean, col=colors[1], pch=22, xlab='', cex=1, bg='white')
points(time_df3$plot_nu[which(time_df3$Order==2)], time_df3$C13mean[which(time_df3$Order==2)], col=colors[1], pch=22, xlab='', cex=1, bg=colors[1])

abline(v=max(time_df3$plot_nu[which(time_df3$Order<1.5)])+0.5, lwd=1)

box(which='plot')

axis(2, las=1, cex.axis=0.7)
axis(1, at=time_df3$plot_nu, labels=time_df3$LakeName, cex.axis=0.65, las=3)

# mtext( expression(paste(delta^{13}, "C POM (\u2030)")), side=2, line=1.5)
mtext( "Lake", side=1, line=3.5)

dev.off()




