### =========================================================================
### Initialise system
### =========================================================================
# Start with house cleaning
rm(list = ls()); graphics.off()

library(terra)
library(RColorBrewer)
library(magick)
library(tidyverse)
library(viridis)
library(sp)
library(raster)
library(wsl.plot)
library(RColorBrewer)
library(ggplot2)


options(scipen=999)

setwd("L:/ELE_projects/beccy/Sister Species_zili/chapter1/writing/parts draft/chapter1_final/Chapter_1_0410")

### =================================
### load in data for the project
### =================================

Hds_county <- vect('./File/HDS_county.shp') 
Hds_county_outline <- aggregate(Hds_county)

ele_raw <- rast("./File/250m_UTM47.tif")
ele_raw <- crop(ele_raw, Hds_county_outline)
crs(ele_raw) <- CRS("+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs +type=crs")

ele <- rast("./File/1000m_UTM47_DEM.tif")

river <- vect("./File/river.shp")
river <- terra::project(river, "+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs +type=crs")
river <- crop(river, Hds_county_outline)
river <- mask(river, Hds_county_outline)

relief <- rast("./File/Geo_07_relief_above_500m.tif")

end_div <- rast("./File/end_div.tif")
end_div <- crop(end_div, Hds_county_outline)
end_div <- mask(end_div, Hds_county_outline)

glacial <- rast("./File/glacial.tif")
glacial[glacial == 0] <- NA
glacial <- crop(glacial, Hds_county_outline)
glacial <- mask(glacial, Hds_county_outline)

faults <- vect("./File/AFEAD_HDM_faults.shp")

### =========================================================================
### Prep decorations
### =========================================================================

slp <- terra::terrain(ele_raw, v = "slope", unit="radians")
asp <- terrain(ele_raw, v = "aspect", unit="radians")
hillsh <-shade(slp, asp,direction=315)
hillcol=colorRampPalette(c("#00000000","#000000DA"),alpha=TRUE)

col_vec <- colorRampPalette(c("#9ECAE1","#0099CC", "#58BC5D","#EEF559","#FF9933","red"))(100)
ETH_blue_old <- "#1F407A"
ETH_blue_new <- "#215CAF"

### =========================================================================
### Plot endemic richness Figure 1 A
### =========================================================================

## ===== labels ====
rn=c(0,773)

lab=c('0' = 0,
      "150"=150,
      "300"=300,
      "450"=450,
      "600"=600,
      "750"=750)

## =================

png("output/F1A_endemic_richness.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(end_div,col =col_vec,legend=F,
     plg = list(title = "Endemic richness"),
     axes=F,mar=c(0.1,0.1,0.1,4.2), border=F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)
plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)

par(xpd=NA)
cscl(colors=col_vec,
     crds=c(1220000,1250000,2800000, 3600000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Endemic richness",
     lablag=-.5,
     titlag=-1.8,
     tickle=-1,
     cx=1,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('a',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

legend(-150000,2900000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 0.9)

dev.off()

gc()

### =========================================================================
### Plot elevation Figure 1 b
### =========================================================================

summ <- read.csv("./File/ele_summ.csv",
                 row.names = 1)

summ.1c <- summ[, c(8,9,15)]

min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

summ.1c <- as.data.frame(lapply(summ.1c, min_max_scale))
summ.1c$ele <- summ$ele
#summ.1c$por <- summ$div /summ$all_div

summ.1c.bar <- summ.1c$area %>% as.matrix()
new_row_names <- summ.1c$ele
rownames(summ.1c.bar) <- new_row_names


png("output/f1b.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

ggplot(summ.1c, aes(x = ele)) +
  geom_bar(aes(y = area, fill = 'Area'),stat="identity", color="white")+
  geom_line(aes(y = div, color = "Endemic"), size = 1) +   # thicker line
  geom_line(aes(y = all_div, color = "Overall"), size = 1) +  # thicker line
  #geom_line(aes(y = por, color = "%endemic"), size = 1) +
  xlab("Elevation (m)")+
  ylab("Standardized value")+
  labs(color = "Richness type", fill = 'Area' , tag = "b") +
  theme_bw() +   # white background
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.3, 'cm'), 
        legend.text = element_text(size=3), 
        legend.title = element_text(size=5), 
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'), 
        legend.box.margin=margin(-10,-10,-10,-10)) +  # remove panel border and grid, add axis line
  scale_color_manual(values = c("Endemic" = "#0099CC", "Overall" = "#FF9933", 
                                #"%endemic"= 'grey', 
                                'Area' = 'lightpink'))

dev.off()

### =========================================================================
### Plot glaciation Figure 1 c
### =========================================================================

## ===== labels ====
rn=c(0,7500)

lab=c('0' = 0,
      "1500"=1500,
      "3000"=3000,
      "4500"=4500,
      "6000"=6000,
      "7500"=7500)
## =================

png("output/glaciation_f1c.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(ele_raw,col =terrain.colors(50),legend=F,
     axes=F,mar=c(0.1,0.1,0.1,4.2), plg = list(title = "Elevation(m)"), border=T , alpha = 0.9)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(glacial,col=ETH_blue_new,add=T,border=F, lwd = 1.5, legend=F)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)

par(xpd=NA)
cscl(colors=terrain.colors(50),
     crds=c(1220000,1250000,2800000, 3600000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Elevation (m)",
     lablag=-.5,
     titlag=-1.8,
     tickle=-1,
     cx=0.78,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('c',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

legend(-150000,2900000, legend = 'LGM glacier', fill = ETH_blue_new, border = 'white', box.lty=0, cex = 0.9)
legend(-150000,2800000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 0.9)


dev.off()
gc()


### =========================================================================
### Plot elevation Figure 1 d
### =========================================================================

ele_raw1 <- ele_raw

ele_raw1[ele_raw1 < 1800] <- 1
ele_raw1[ele_raw1 >= 1800 & ele_raw1 < 2800 ] <- 2
ele_raw1[ele_raw1 >= 2800 & ele_raw1 < 3800 ] <- 3
ele_raw1[ele_raw1 >= 3800 & ele_raw1 < 4600 ] <- 4
ele_raw1[ele_raw1 >= 4600] <- 5

#glacial1 <- resample(glacial, ele_raw1, method = 'near')
#ele_raw1[glacial1 >= 1] <- NA

col_terrain2 <- colorRampPalette(c("#008837","#91cf60", "yellow","#fc8d59", '#392a48'))(5)

png("output/ele_f1d.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)
plot(ele_raw1,col =col_terrain2,legend=F,
     axes=F,mar=c(0.1,0.1,0.1,4.2), plg = list(title = "Elevation(m)"), border=T , alpha = 0.9)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)

par(xpd=NA)
# box(lwd=0.5)
panel.let('d',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

legend(-150000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 0.9)
legend(-150000,2850000, legend = c('below 1800m','1800-2800m','2800-3800m','3800-4600m','above 4600m'), 
       fill = col_terrain2, border = 'white', box.lty=0, cex = 0.9)

dev.off()

gc()

### ===========================================================================
### Data prep for figure 2
### =========================================================================


ele <- rast("./File/1000m_UTM47_DEM.tif")
Flux <- rast("./File/flux_mean_d10.tif")
velo <- rast("./File/Velo.tif")
tem <- rast("./File/tem.tif")
swb <- rast("./File/swb.tif")
end_div <- rast("./File/end_div.tif")

flux_all <- data.frame(
  ele = values(ele),
  end_div = values(end_div),
  tem = values(tem),
  swb = values(swb),
  Flux = values(Flux),
  velo = values(velo) %>% abs()
) 

names(flux_all) <- c("ele", "end_div", "tem", 'swb', 'Flux', 'velo')

flux_all <- flux_all %>% 
  cbind(ele %>%
          xyFromCell(., 1:ncell(ele)) %>%
          as.data.frame()) %>%
  na.omit() %>% 
  filter(is.finite(Flux))

## Fig 2A prep ##
all_glm <- glm(
  end_div ~ tem + swb + log(velo) + I(tem^2) + I(swb^2)+ I(log(velo)^2),
  family = "poisson",
  data = flux_all) %>% 
  step(trace = F)
flux_all$resi <- residuals(all_glm)

res_rel_imp <- raster(tem)
values(res_rel_imp) <- NA

for (xmin in seq(-248500, 1051500, 50000)) {
  cat(xmin, "\n")
  for (ymin in seq(2600000, 3750000, 50000)) {
    cat(ymin, "\r")
    flux_df <- flux_all[flux_all$x > (xmin + 25000) & flux_all$x < (xmin + 75000),]
    flux_df <- flux_df[flux_df$y > (ymin + 25000) & flux_df$y < (ymin + 75000),]
    
    flux_df <- na.omit(flux_df)
    flux_df <- flux_df[is.finite(flux_df$Flux),]
    
    flux_df <- flux_df[flux_df$ele>1000 & flux_df$ele<5000, ] %>%
      na.omit()
    
    if(nrow(flux_df)<50){
      next
    }
    
    extent_window <- extent(c(xmin + 25000, xmin + 75000, 
                              ymin + 25000, ymin + 75000))
    res_rel_imp[extent_window] <- cor(flux_df$resi, flux_df$Flux)
  }
}

res_rel_imp <- rast(res_rel_imp)

### =========================================================================
### Plot flux vs richness Pearson's R figure 2 A
### =========================================================================
## ===== labels ====
rn=c(-1,1)

lab=c('-1' = -1,
      "-0.5"=-0.5,
      "0"=0,
      "0.5"=0.5,
      "1"=1)
## =================

png("output/richvsflux_f2a.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(res_rel_imp %>% mask(Hds_county_outline), 
     range = c(-1, 1), 
     col = colorRampPalette(c("dodgerblue", "white", "firebrick1"))(16),
     plg = list(title = "Pearson's r"),
     mar=c(0.5,0.1,0.1,4.2), axes=F, border=T , alpha = 1, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)

par(xpd=NA)
cscl(colors=colorRampPalette(c("dodgerblue", "white", "firebrick1"))(16),
     crds=c(1220000,1250000,2800000, 3600000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Pearson's r",
     lablag=-.5,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('a',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

legend(-150000,2900000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 0.9)

dev.off()

### =========================================================================
### Calculating number of windows
### =========================================================================
res_rel_imp_negative <- res_rel_imp
res_rel_imp_negative <- aggregate(res_rel_imp_negative, fact=50, fun=mean, na.rm=TRUE)
res_rel_imp_negative[values(res_rel_imp_negative) >= 0] <- NA
res_rel_imp_negative <- crop(res_rel_imp_negative, end_div)

end_div_hotspot <- end_div
end_div_hotspot <- end_div_hotspot %>% log()
th <- terra::minmax(end_div_hotspot)*0.9
end_div_hotspot[values(end_div_hotspot) <= th[2]] <- NA


end_div_hotspot0 <- terra::resample(end_div_hotspot, res_rel_imp_negative, method = 'bilinear')

end_div_hotspot0_masked <- end_div_hotspot0
th1 <- terra::minmax(res_rel_imp_negative)
end_div_hotspot0_masked[values(res_rel_imp_negative) <= -0.000000000000000001] <- NA

pixel_freq <- terra::freq(end_div_hotspot0, digits = 10)
non_na_pixel_count <- sum(pixel_freq[, "count"], na.rm = TRUE)

pixel_freq <- terra::freq(end_div_hotspot0_masked, digits = 10)
non_na_pixel_count1 <- sum(pixel_freq[, "count"], na.rm = TRUE)

(non_na_pixel_count-non_na_pixel_count1)/non_na_pixel_count

##### calculate for glacial map 

distance_raster <- terra::distance(glacial)
distance_raster[distance_raster == 0] <-NA
th2 <- terra::minmax(distance_raster)
distance_raster[distance_raster >= th2*0.1] <- NA
distance_raster0 <- terra::resample(distance_raster, res_rel_imp_negative, method = 'bilinear')

pixel_freq <- terra::freq(distance_raster0, digits = 10)
non_na_pixel_count2 <- sum(pixel_freq[, "count"], na.rm = TRUE)

distance_raster0_masked <- distance_raster0
distance_raster0_masked[values(res_rel_imp_negative) <= -0.000000000000000001] <- NA

pixel_freq <- terra::freq(distance_raster0_masked, digits = 10)
non_na_pixel_count3 <- sum(pixel_freq[, "count"], na.rm = TRUE)

(non_na_pixel_count2-non_na_pixel_count3)/non_na_pixel_count2

### =========================================================================
### Plot  figure 2 B correlation on elevation 
### =========================================================================
library(plot3D)
library(lattice)
library(latticeExtra)
library(vip)
library(grid)
library(gridExtra)


plot_5km <- function(i, summ){
  lm_5km <- lm(eval(parse(text = paste("resi~", i, sep = ""))), data = summ %>%
                 scale() %>%
                 as.data.frame())
  lm_coeff <- summary(lm_5km)$coefficient
  lm_r <- summary(lm_5km)$r.squared
  
  if(round(lm_coeff[2,4], 3) == 0){
    p_text <- "p-value < 0.01"
  }
  else{
    p_text <- paste("p-value = ", 
                    round(lm_coeff[2,4], 3), 
                    sep = "")
  }
  p_text <- paste("Coef = ",
                  round(lm_coeff[2,1], 3),
                  "\n",
                  "R sqr = ", 
                  round(lm_r, 3), 
                  "\n",
                  p_text, 
                  sep = "")
  
  obj1 <- xyplot(resi ~ ele, data = summ, type = "l" , lwd=2, col="steelblue",
                 scales = list(tck = 0), xlab = "Elevation (m)",
                 ylab = "Residuals",
                 panel=function(x, y, ...) {
                   panel.xyplot(x, y, ...);
                   ltext((min(summ$ele) + 500), 0.65*max(summ$div), 
                         labels=p_text, cex=0.9)
                 })
  
  obj2 <- xyplot(eval(parse(text = paste(i, " ~ ele",  sep = ""))), 
                 data = summ, type = "l", lwd=2, col="orange",
                 scales = list(tck = 0), xlab = "Elevation (m)",
                 ylab = ifelse(i == "PC",
                               i,
                               parse(text = paste("Cost~distance[", gsub("q_", "Q[", i) , "]]" , sep = "")%>%
                                       gsub(pattern = "n]]", replacement = "n]"))))
  
  fig <- doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE)
  return(fig)
}

summ <- read.csv("./File/ele_summ.csv",
                 row.names = 1)

sar <- lm(log(div) ~ log(area), data = summ)
summ$resi <- summ$div - exp(sar$fitted.values)

sar <- lm(div ~ all_div, data = summ)
summ$resi_all <- residuals(sar)

colnames(summ)[colnames(summ) == "q_50"] <- "Median"
colnames(summ)[colnames(summ) == "mean"] <- "Mean"
fig_1 <- plot_5km("Mean", summ)
fig_2 <- plot_5km("q_05", summ)
fig_3 <- plot_5km("q_25", summ)
fig_4 <- plot_5km("Median", summ)
fig_5 <- plot_5km("q_75", summ)
fig_6 <- plot_5km("q_95", summ)

png("output/f2b.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)

grid.arrange(fig_4, ncol = 1,
             vp=viewport(width = 1, height=1, just = 0.5, gp = gpar(fontsize = 7, lwd = 1, cex = 0.5)))
KeyA<-list(space="right",
           lines=list(lwd = 2, col = c("steelblue", "orange")),
           text=list(c("Residuals","Cost distance")), padding.text=5)
KeyB<-list(space="right",
           text=list('b'), padding.text=5)
draw.key(KeyA, draw = TRUE, vp = viewport(.25, .7, gp = gpar(fontsize = 5, lwd = 1)))
draw.key(KeyB, draw = TRUE, vp = viewport(0.05, .95, gp = gpar(fontsize = 15, lwd = 1)))

dev.off()

###Supp3###
png("output/supp3.png",width=17,height=14,unit="cm",res=300,pointsize=7)
grid.arrange(fig_1,fig_2,fig_3,fig_4,fig_5,fig_6, ncol = 3,
             vp=viewport(width = 0.9, height=1, just = 0.55, gp = gpar(fontsize = 7, lwd = 0.8, cex = 0.4)))
KeyA<-list(space="right",
           lines=list(lwd = 2, col = c("steelblue", "orange")),
           text=list(c("Residuals","Cost distance")), padding.text=10)
draw.key(KeyA, draw = TRUE, vp = viewport(.9, .50), gp = gpar(fontsize = 3, lwd = 0.5,  cex = 0.2))
dev.off()

### =========================================================================
### Plot  figure 2 c ols on ele
### =========================================================================
summ <- read.csv("./File/ele_summ.csv",
                 row.names = 1)

sar <- lm(log(div) ~ log(area), data = summ)
summ$resi <- summ$div - exp(sar$fitted.values)

sar <- lm(div ~ all_div, data = summ)
summ$resi_all <- residuals(sar)

all_glm <- lm(
  resi ~ q_50 + tem + I(tem^2) + velo + I(velo^2),
  data = summ)  

all_imp <- all_glm %>% relaimpo::calc.relimp(rela = T)
all_imp <- all_imp@lmg
all_imp <- all_imp/sum(all_imp)
all_imp <- sort(all_imp, decreasing = T)
names(all_imp)[names(all_imp) == "velo"] <- "ClimVelo"
names(all_imp)[names(all_imp) == "I(velo^2)"] <- "I(ClimVelo^2)"

all_imp <- data.frame(
  Linear = all_imp[c("q_50", "tem", "ClimVelo")],
  Quadratic  = c(0, all_imp["I(tem^2)"], all_imp["I(ClimVelo^2)"])
)

png("output/f2c.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)
barplot(all_imp %>% as.matrix %>% t(), 
        col = c("steelblue1", "lightpink"),
        names = all_imp %>% rownames()%>%
          gsub(pattern = "tem", replacement = "MTWQ")%>%
          gsub(pattern = "swb", replacement = "SWB") %>%
          gsub(pattern = "q_50", replacement = "Cost ~ Distance[Median]") %>%
          gsub(pattern = "q_75", replacement = "Cost ~ Distance[Q[75]]") %>% 
          gsub(pattern = "q_25", replacement = "Cost ~ Distance[Q[25]]") %>%
          gsub(pattern = "q_05", replacement = "Cost ~ Distance[Q[05]]") %>% 
          gsub(pattern = "q_95", replacement = "Cost ~ Distance[Q[95]]") %>%
          gsub(pattern = "mean", replacement = "Cost ~ Distance[Mean]") %>%
          parse(file = NULL, n = NULL),
        xlab = "Predictors",
        ylab = "Relavtive importance",
        legend = all_imp %>% colnames(),
        args.legend = list(bty = "n", x = 3.6, y = 0.6)
)
text(nrow(all_imp) + 0.1, 0.75*max(all_imp), 
     paste("R squared = ", summary(all_glm)$r.squared %>%
             round(3), sep = ""))
par(xpd=NA)
# box(lwd=0.5)
panel.let('c',font=1,shift=c(-0.09,-0.15), cex = 2)
par(xpd=F)


dev.off()

### ========================================================================
### Data prep for figure 3
### ========================================================================

ETH_blue_old <- "#1F407A"

beta_cata <- rast("./File/beta_cata.tif")
hotspot <- extent(360000, 880000, 2850000, 3500000)

#12 classes
my_color4 <- colorRampPalette(colors = c('#73ABAB', '#BBE6E4',
                                         '#2FB8DA', '#074373',
                                         '#F57600', '#C44E00',
                                         '#A36912', '#663A00',
                                         '#E0E000', '#E0BB00',
                                         '#ff0505', '#b51936'))


png("output/beta_cata_fig3a.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(beta_cata, 
     col = my_color4(12),
     plg = list(title = "composition catagories"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=T , alpha = 1 , legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)

plot(faults,col='black',add=T,border=F, lty=2, lwd = 2)

#par(xpd=NA)
#legend("topright", legend = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'), fill = )
#par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('a',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
legend(280000,2900000, legend = "Faults", col = "black", lty = 2, lwd = 2, box.lty=0,cex = 1, bg="transparent")
par(xpd=F)

dev.off()
gc()

### ========================================================================
### categories fig3a
### ========================================================================

######################
library(dendextend)
#####################
sp_nested_turnover <- readRDS("./File/sp_nested_turnover.rds")
hc <- hclust(sp_nested_turnover, method = 'average')
hc_dendro <- as.dendrogram(hc)
hc_dendro_cut <- cut(hc_dendro, h= 0.38)
hc_dendro_upper <- hc_dendro_cut$upper
tree1 <- as_hclust_fixed(hc_dendro_upper)
tree1 <- as.dendrogram(tree1)

rm(sp_nested_turnover)
gc()
#tree1 <- flatten.dendrogram(tree1)

ForceUltrametric <- function(n) {
  if (is.leaf(n)) {
    # if object is a leaf, adjust height attribute
    attr(n, "height") <- 0.35
  }
  return(n)
}

tree1 <- dendrapply(X = tree1,
                    FUN = function(x) ForceUltrametric(x))

#write.tree(hc_phylo0, file = "12_cata_slice_tree.newick")

labels(tree1) <- as.character(seq(1:12))

png("output/beta_cata_fig3a_section.png",width=3.95,height=6.9,unit="cm",res=300,pointsize=7.5)
par(mar=c(1, 1, 1, 1)) 
plot_horiz.dendrogram(tree1, horiz = TRUE, axes = F, xlim = c(0.8, 0.2), mar=c(0.01,0.01,0.01,0.01))

#plot(tree1, horiz = TRUE, axes = F, xlim = c(0.8, 0.2), mar=c(0.01,0.01,0.01,0.01))

colored_bars(colors =  my_color4(12), tree1, 
             rowLabels = ' ', 
             horiz = TRUE, y_shift = -0.25)


tree1 %>% rect.dendrogram(k = 3, horiz = TRUE, border = 8, lty = 5, lwd = 2, cex =0.7)

par(xpd=NA)
# box(lwd=0.5)
text(0.84, 2.5, 'Group 1', srt=90, cex =1)
text(0.84, 5.5, 'Group 2', srt=90, cex =1)
text(0.84, 9.5, 'Group 3', srt=90, cex =1)
par(xpd=F)

dev.off()

### ================================================
### Elevation density figure 3 B
### ================================================
hotspot <- extent(361000, 880000, 2853000, 3498000)
ele_focal <- mask(ele, Hds_county_outline)
ele_focal <- crop(ele_focal, hotspot)

ele_cal <- terra::resample(ele, beta_cata, method = 'max')
ele_cal <- mask(ele_cal, Hds_county_outline)

resultRaster1 <- ele_cal[beta_cata == 1]
resultRaster2 <- ele_cal[beta_cata == 2]
resultRaster3 <- ele_cal[beta_cata == 3]
resultRaster4 <- ele_cal[beta_cata == 4]
resultRaster5 <- ele_cal[beta_cata == 5]
resultRaster6 <- ele_cal[beta_cata == 6]
resultRaster7 <- ele_cal[beta_cata == 7]
resultRaster8 <- ele_cal[beta_cata == 8]
resultRaster9 <- ele_cal[beta_cata == 9]
resultRaster10 <- ele_cal[beta_cata == 10]
resultRaster11 <- ele_cal[beta_cata == 11]
resultRaster12 <- ele_cal[beta_cata == 12]

resultgroup1 <- rbind(resultRaster1,resultRaster2, resultRaster3, resultRaster4)
resultgroup2 <- rbind(resultRaster5,resultRaster6)
resultgroup3 <- rbind(resultRaster7,resultRaster8, resultRaster9, resultRaster10, resultRaster11, resultRaster12)


png("output/beta_cata_fig3a_sectionb1.png",width=4,height=6.9,unit="cm",res=300,pointsize=7.5)
par(mfrow = c(1, 1), mar=c(2, 2, 1, 1))

dens <- density(resultgroup1)
plot(dens, frame = FALSE, col = my_color4(12)[4], lwd = 1.2, xlim = c(1000, 6500), ylim = c(0, 0.0015), main = NA, xlab=NA,ylab=NA) 

dens <- density(resultgroup2)
lines(dens, frame = FALSE, col = my_color4(12)[5], lwd = 1.2, xlim = c(1000, 6500),ylim = c(0, 0.0015), main = NA, ylab=NA, xlab=NA, add= T) 

dens <- density(resultgroup3)
lines(dens, frame = FALSE, col = my_color4(12)[11], lwd = 1.2, xlim = c(1000, 6500),ylim = c(0, 0.0015), main = NA, ylab=NA, xlab=NA, add= T) 

par(xpd=NA)
# box(lwd=0.5)
text(6300, -0.00015, 'Elevation
     (m)', cex=0.8)
par(xpd=F)


legend("topright", legend = c("Group 1","Group 2","Group 3") , col = c(my_color4(12)[4],my_color4(12)[5],my_color4(12)[11]), 
       cex=0.8, box.lty=0, lty = 1, lwd = 2)


par(xpd=NA)
# box(lwd=0.5)
text(1300, 0.00155, 'b', font=1, cex = 2)
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
text(1800, 0.0013, 'Density', cex=0.8)
par(xpd=F)

dev.off()


################################################################################
######## beta window, fig 3 c######
#########################################################

## Moving window
NMDS1_map <- raster("./File/NMDS1_map.tif")
NMDS2_map <- raster("./File/NMDS2_map.tif")
NMDS3_map <- raster("./File/NMDS3_map.tif")
beta_map <- raster("./File/beta_map.tif")
names(beta_map) <- "beta"

ID <- raster("./File/ID.tif") %>%
  projectRaster(to = beta_map)
values(ID) <- 1:ncell(ID)

tem <- raster("./File/tem.tif") %>%
  projectRaster(to = beta_map)
names(tem) <- "tem"

swb <- raster("./File/swb.tif") %>%
  projectRaster(to = beta_map)
names(swb) <- "swb"

velo <- raster("./File/velo.tif") %>%
  projectRaster(to = beta_map)
names(velo) <- "velo"

flux <- raster("./File/flux_mean_d10.tif") %>%
  projectRaster(to = beta_map)
names(flux) <- "flux"

beta_df <- stack(ID, beta_map, tem, swb, velo, flux) %>%
  rasterToPoints() %>%
  as.data.frame() %>%
  na.omit

glm_all <- glm(beta ~ tem + I(tem^2) + swb + I(swb^2) + velo + I(velo^2) + flux + I(flux^2),
               data = beta_df,
               family = "binomial")

imp_summ <- glm_all %>% caret::varImp()
imp <- structure(imp_summ[,1], names = rownames(imp_summ))
imp <- imp/sum(imp)
imp <- sort(imp, decreasing = T)
coef <- summary(glm_all)$coefficients[,1]
coef <- coef[names(imp)]

glm_all <- glm(beta ~ tem + I(tem^2) + swb + I(swb^2) + velo + I(velo^2),
               data = beta_df,
               family = "binomial")

beta_df$resi <- residuals(glm_all)

res_rel_imp <- raster(beta_map)

for (xmin in seq(362000, 842000, 30000)) {
  cat(xmin, "\n")
  for (ymin in seq(2854000, 3454000, 30000)) {
    cat(ymin, "\r")
    beta_df_mv <- beta_df[beta_df$x > (xmin - 1) & beta_df$x < (xmin + 30000),]
    beta_df_mv <- beta_df_mv[beta_df_mv$y > (ymin - 1) & beta_df_mv$y < (ymin + 30000),]
    
    if(nrow(beta_df_mv) > 4){
      extent_window <- extent(c(xmin, xmin + 30000, 
                                ymin, ymin + 30000))
      res_rel_imp[extent_window] <- cor(beta_df_mv$flux, beta_df_mv$resi)
    }
  }
}

res_rel_imp <- rast(res_rel_imp)

### =========================================================================
### Plot flux vs beta Pearson's R figure 3 c
### =========================================================================
rn=c(-1,1)

lab=c('-1' = -1,
      "-0.5"=-0.5,
      "0"=0,
      "0.5"=0.5,
      "1"=1)

png("output/betavsflux_f3c.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(res_rel_imp %>% terra::mask(.,Hds_county_outline), 
     range = rn, 
     col = colorRampPalette(c("dodgerblue", "white", "firebrick1"))(16),
     plg = list(title = "Pearson's r"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=T , alpha = 1, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 1.5)



par(xpd=NA)
cscl(colors=colorRampPalette(c("dodgerblue", "white", "firebrick1"))(16),
     crds=c(955000, 970000, 3000000, 3400000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Pearson's r",
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('c',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
par(xpd=F)

dev.off()
gc()

### =========================================================================
### Plot NDMS with faults and rivers figure 3 c
### =========================================================================
NMDS3_map <- raster("./File/NMDS3_map.tif")
NMDS3_map <- rast(NMDS3_map)

# Scale the raster to the desired range
scaled_raster <- ((NMDS3_map - minmax(NMDS3_map)[1]) / (minmax(NMDS3_map)[2] - minmax(NMDS3_map)[1]) - 0.5) * 2

rn=c(-1,1)

lab=c('-1' = -1,
      "-0.5"=-0.5,
      "0"=0,
      "0.5"=0.5,
      "1"=1)

NDMS_col <- colorRampPalette(c(
  '#543005',
  '#8c510a',
  '#bf812d',
  '#dfc27d',
  '#f6e8c3',
  '#f5f5f5',
  '#c7eae5',
  '#80cdc1',
  '#35978f',
  '#01665e',
  '#003c30'
))(11)

#ETH_blue_old <- '#0437f2'
ETH_blue_old <- ETH_blue_new

png("output/NDMS_fig3D.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(scaled_raster %>% terra::mask(.,Hds_county_outline), 
     range = rn, 
     col = NDMS_col,
     plg = list(title = "NDMS3"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=T , alpha = 0.8, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 2)

plot(faults,col='black',add=T,border=F, lty=2, lwd = 2)


par(xpd=NA)
cscl(colors=NDMS_col,
     crds=c(955000, 970000, 3000000, 3400000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title='NMDS axis 3 value',
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('d',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
legend(280000,2900000, legend = "Faults", col = "black", lty = 2, lwd = 2, box.lty=0,cex = 1, bg="transparent")
par(xpd=F)

dev.off()
gc()

### =========================================================================
### Plot NDMS with faults and rivers suppl figure 6
### =========================================================================
NMDS3_map <- raster("./File/NMDS1_map.tif")
NMDS3_map <- rast(NMDS3_map)

# Scale the raster to the desired range
scaled_raster <- ((NMDS3_map - minmax(NMDS3_map)[1]) / (minmax(NMDS3_map)[2] - minmax(NMDS3_map)[1]) - 0.5) * 2

rn=c(-1,1)

lab=c('-1' = -1,
      "-0.5"=-0.5,
      "0"=0,
      "0.5"=0.5,
      "1"=1)

png("output/suppl_fig6a_ndms1.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(scaled_raster %>% terra::mask(.,Hds_county_outline), 
     range = rn, 
     col = NDMS_col,
     plg = list(title = "NDMS3"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=T , alpha = 0.8, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 2)

plot(faults,col='black',add=T,border=F, lty=2, lwd = 2)


par(xpd=NA)
cscl(colors=NDMS_col,
     crds=c(955000, 970000, 3000000, 3400000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title='NMDS axis 1 value',
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('a',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
legend(280000,2900000, legend = "Faults", col = "black", lty = 2, lwd = 2, box.lty=0,cex = 1, bg="transparent")
par(xpd=F)

dev.off()
gc()

#===================================
NMDS3_map <- raster("./File/NMDS2_map.tif")
NMDS3_map <- rast(NMDS3_map)

# Scale the raster to the desired range
scaled_raster <- ((NMDS3_map - minmax(NMDS3_map)[1]) / (minmax(NMDS3_map)[2] - minmax(NMDS3_map)[1]) - 0.5) * 2

rn=c(-1,1)

lab=c('-1' = -1,
      "-0.5"=-0.5,
      "0"=0,
      "0.5"=0.5,
      "1"=1)

png("output/suppl_fig6a_ndms2.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(scaled_raster %>% terra::mask(.,Hds_county_outline), 
     range = rn, 
     col = NDMS_col,
     plg = list(title = "NDMS3"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=T , alpha = 0.8, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 2)

plot(faults,col='black',add=T,border=F, lty=2, lwd = 2)


par(xpd=NA)
cscl(colors=NDMS_col,
     crds=c(955000, 970000, 3000000, 3400000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title='NMDS axis 2 value',
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
# box(lwd=0.5)
panel.let('c',font=1,shift=c(0.05,0.05), cex = 2)
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
legend(280000,2900000, legend = "Faults", col = "black", lty = 2, lwd = 2, box.lty=0,cex = 1, bg="transparent")
par(xpd=F)

dev.off()
gc()

### =========================================================================
### Flux plot supplementary fig 1
### =========================================================================
rn=c(0,1)

lab=c('0' = 0,
      "0.2"=0.2,
      "0.4"=0.4,
      "0.6"=0.6,
      "0.8"=0.8, 
      '1'=1)

png("output/supp1a.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(Flux %>% rast() %>%  mask(Hds_county_outline), 
     #range = rn, 
     col = c(colorRampPalette(c("steelblue1", "khaki1", "orangered"))(85)) %>%
       unique(),
     plg = list(title = "NDMS3"),
     mar=c(0.1,0.1,0.1,4.2), axes=F, border=T , alpha = 1, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 2)

par(xpd=NA)
cscl(colors=c(colorRampPalette(c("steelblue1", "khaki1", "orangered"))(85)) %>%
       unique(),
     crds=c(1220000,1250000,2800000, 3600000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Flux",
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
legend(-150000,2900000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 0.9, bg="transparent")
par(xpd=F)

dev.off()

### =========================================================================
### Flux plot supplementary 6
### =========================================================================
ele <- raster("./File/1000m_UTM47_DEM.tif")
end_div <- raster("./File/end_div.tif")
Flux <- raster("./File/flux_mean_d10.tif")
velo <- raster("./File/Velo.tif")
tem <- raster("./File/tem.tif")
swb <- raster("./File/swb.tif")

flux_all <- data.frame(
  ele = values(ele),
  end_div = values(end_div),
  tem = values(tem),
  swb = values(swb),
  Flux = values(Flux),
  velo = values(velo) %>% abs()
) %>% 
  cbind(ele %>%
          coordinates() %>%
          as.data.frame()) %>%
  na.omit() %>%
  filter(is.finite(Flux))

all_glm <- glm(
  end_div ~ tem + swb + log(velo) + Flux + I(tem^2) + I(swb^2)+ I(log(velo)^2) + I(Flux^2),
  family = "poisson",
  data = flux_all) 

all_imp <- all_glm %>% caret::varImp()
all_imp <- structure(all_imp[,1], names = rownames(all_imp))
all_imp <- all_imp/sum(all_imp)
all_imp <- sort(all_imp, decreasing = T)
names(all_imp)[names(all_imp) == "log(velo)"] <- "ClimVelo"
names(all_imp)[names(all_imp) == "I(log(velo)^2)"] <- "I(ClimVelo^2)"

all_imp <- data.frame(
  Linear = all_imp[c("tem", "Flux", "ClimVelo", "swb")],
  Quadratic  = all_imp[c("I(tem^2)", "I(Flux^2)","I(ClimVelo^2)", "I(swb^2)")]
)

png("output/supp2a.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)
par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)
barplot(all_imp %>% as.matrix %>% t(), 
        col = c("steelblue1", "lightpink"),
        xlab = "Predictors",
        ylab = "Relavtive importance",
        names = all_imp %>% rownames() %>%
          gsub(pattern = "tem", replacement = "MTWQ")%>%
          gsub(pattern = "swb", replacement = "SWB") %>%
          parse(file = NULL, n = NULL),
        legend = all_imp %>% colnames(),
        args.legend = list(bty = "n", x = 5, y = 0.6))
text(nrow(all_imp) +0.0, 0.75*max(rowSums(all_imp)), 
     paste("D squared = ", all_glm %>%
             ecospat::ecospat.adj.D2.glm() %>%
             round(3), sep = ""), cex = 1)

title(main="Endemic species richness")

par(xpd=NA)
# box(lwd=0.5)
panel.let('a',font=1,shift=c(-0.09,-0.15), cex = 2)
par(xpd=F)

dev.off()

# apply for beta

beta_map <- raster("./File/beta_map.tif")
ele1 <- resample(ele, beta_map) %>% rast() %>% raster::mask(Hds_county_outline) %>% raster()
tem1 <- resample(tem, beta_map) %>% rast() %>% raster::mask(Hds_county_outline) %>% raster()
swb1 <- resample(swb, beta_map) %>% rast() %>% raster::mask(Hds_county_outline) %>% raster()
Flux1 <- resample(Flux, beta_map) %>% rast() %>% raster::mask(Hds_county_outline) %>% raster()
velo1 <- resample(velo, beta_map) %>% rast() %>% raster::mask(Hds_county_outline) %>% raster()

flux_all <- data.frame(
  ele = values(ele1),
  beta = values(beta_map),
  tem = values(tem1),
  swb = values(swb1),
  Flux = values(Flux1),
  velo = values(velo1) %>% abs()
) %>% 
  cbind(ele1 %>%
          coordinates() %>%
          as.data.frame()) %>%
  na.omit() %>%
  filter(is.finite(Flux))

all_glm <- glm(
  beta ~ tem + swb + log(velo) + Flux + I(tem^2) + I(swb^2)+ I(log(velo)^2) + I(Flux^2),
  family = "binomial",
  data = flux_all)

all_imp <- all_glm %>% caret::varImp()
all_imp <- structure(all_imp[,1], names = rownames(all_imp))
all_imp <- all_imp/sum(all_imp)
all_imp <- sort(all_imp, decreasing = T)
names(all_imp)[names(all_imp) == "log(velo)"] <- "ClimVelo"
names(all_imp)[names(all_imp) == "I(log(velo)^2)"] <- "I(ClimVelo^2)"

all_imp <- data.frame(
  Linear = all_imp[c("tem", "Flux", "ClimVelo", "swb")],
  Quadratic  = all_imp[c("I(tem^2)", "I(Flux^2)","I(ClimVelo^2)", "I(swb^2)")]
)

all_imp <- all_imp[c(1,3,2,4),]

png("output/supp2b.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)
par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)
barplot(all_imp %>% as.matrix %>% t(), 
        col = c("steelblue1", "lightpink"),
        xlab = "Predictors",
        ylab = "Relavtive importance",
        names = all_imp %>% rownames() %>%
          gsub(pattern = "tem", replacement = "MTWQ")%>%
          gsub(pattern = "swb", replacement = "SWB") %>%
          parse(file = NULL, n = NULL),
        legend = all_imp %>% colnames(),
        args.legend = list(bty = "n", x = 5, y = 0.6))
text(nrow(all_imp) +0.0, 0.65*max(rowSums(all_imp)), 
     paste("D squared = ", all_glm %>%
             ecospat::ecospat.adj.D2.glm() %>%
             round(3), sep = ""), cex = 1)

title(main="Endemic species beta diversity")

par(xpd=NA)
# box(lwd=0.5)
panel.let('b',font=1,shift=c(-0.09,-0.15), cex = 2)
par(xpd=F)

dev.off()

### =========================================================================
### Flux plot supplementary 4 beta 
### =========================================================================

beta_log <- beta_map %>% scale() %>% rast()

# Scale the raster to the desired range
rn=c(-1.5,4.5)

lab=c('-1.5' = -1.5,
      "0"=0,
      "1.5"=1.5,
      '3' = 3,
      '4.5' = 4.5)

png("output/supp4.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7.5)
# pdf(fln,width=10/2.54,height=6.54/2.54,useDingbats = F,pointsize=7.5)

par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)

plot(beta_log, 
     range = rn, 
     col = c(colorRampPalette(c("steelblue1", "khaki1", "orangered"))(85)) %>%
       unique(),
     plg = list(title = "NDMS3"),
     mar=c(0.5,0.5,0.1,1), axes=F, border=F , alpha = 1, legend = F)

plot(hillsh,bty="n",legend=F,axes=F,col=rev(hillcol(50)),maxcell=1e8,
     add=T)

plot(Hds_county_outline, add = T, lwd = 1, color = NA)

plot(river,col=ETH_blue_old,add=T,border=F, lwd = 2)

par(xpd=NA)
cscl(colors=c(colorRampPalette(c("steelblue1", "khaki1", "orangered"))(85)) %>%
       unique(),
     crds=c(955000, 970000, 3000000, 3400000),
     zrng=rn,
     at=lab[which(lab>=rn[1] & lab<=rn[2])],
     labs = names(lab)[which(lab>=rn[1] & lab<=rn[2])],
     title="Beta Diversity",
     lablag=-.2,
     titlag=-1.8,
     tickle=-1,
     cx=0.8,
     horiz=F,
     tria="u")
par(xpd=F)

par(xpd=NA)
legend(280000,2950000, legend = "Rivers", col = ETH_blue_old, lty = 1, lwd = 2,box.lty=0, cex = 1, bg="transparent")
par(xpd=F)

dev.off()

### =========================================================================
### Supplementary figure 5
### =========================================================================

NMDS1_map <- raster("./File/NMDS1_map.tif")
NMDS2_map <- raster("./File/NMDS2_map.tif")
NMDS3_map <- raster("./File/NMDS3_map.tif")
beta_map <- raster("./File/beta_map.tif")
names(beta_map) <- "beta"

ele <- raster("./File/1000m_UTM47_DEM.tif")
ele <- ele %>% projectRaster(to = beta_map)
beta_all <- stack(ele, NMDS1_map, NMDS2_map, NMDS3_map, beta_map) %>%
  rasterToPoints() %>%
  as.data.frame() %>%
  na.omit
colnames(beta_all) <- c("x", "y", "ele", 
                        "NMDS1", "NMDS2", "NMDS3", "beta")

summ <- read.csv("./File/ele_summ.csv",
                 row.names = 1)


beta_summ <- data.frame(
  ele=NA,
  beta=NA,
  NMDS1=NA,
  NMDS2=NA,
  NMDS3=NA,
  Max=NA,
  Min=NA
)

for (i in 10:49*100) {
  beta_ele <- beta_all %>%
    filter(ele > i & ele <(i+100))
  beta_summ <- rbind(beta_summ,
                     c(i*1,
                       mean(beta_ele$beta),
                       sd(beta_ele$NMDS1),
                       sd(beta_ele$NMDS2),
                       sd(beta_ele$NMDS3),
                       (mean(beta_ele$beta)+(sd(beta_ele$beta)/sqrt(length(beta_ele$beta)))),
                       (mean(beta_ele$beta)-(sd(beta_ele$beta)/sqrt(length(beta_ele$beta)))) 
                     ))
}

beta_summ <- na.omit(beta_summ)

summ <- merge(summ, beta_summ, by = "ele")

exp_var <- c(
  "mean",
  "q_05",
  "q_25",
  "q_50",
  "q_75",
  "q_95"
)
conn_R2 <- data.frame(
  exp = exp_var,
  R2 = NA
)

for (i in 1:length(exp_var)) {
  conn_lm <- lm(beta ~ ., data = summ[,c("beta", exp_var[i])]) %>% summary()
  conn_R2[i, 2] <- conn_lm$r.squared
}
conn_ind <- conn_R2[which.max(conn_R2$R2), 1]

lm_summ <- lm(beta ~ tem + I(tem^2) + velo + I(velo^2) + q_50, ## Here replace "NMDS3" with "beta"
              data = summ[,c("beta", "q_50", "tem", "swb", "velo")] %>%
                scale() %>%
                as.data.frame())

R2 <- summary(lm_summ)$adj.r.squared

imp_summ <- lm_summ %>% relaimpo::calc.relimp(rela = T)
imp <- imp_summ@lmg
imp <- sort(imp, decreasing = T)
imp <- data.frame(
  Linear = imp[c("tem", "velo", "q_50")],
  Quadratic  = imp[c("I(tem^2)", "I(velo^2)", "")]
)

imp <- imp[c(1,3,2),]

coef <- summary(lm_summ)$coefficients[,1]
coef <- coef[names(imp)]

png("output/suppfig5b.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)
par(mfrow=c(1,1),oma=c(0,0,0,0),ps=8,cex=1)
barplot(imp %>% as.matrix %>% t(), 
        col = c("steelblue1", "lightpink"), 
        names = imp %>% rownames() %>%
          gsub(pattern = "tem", replacement = "MTWQ")%>%
          gsub(pattern = "q_50", replacement = "Cost ~ Distance[Median]") %>%
          gsub(pattern = "velo", replacement = "ClimVelo") %>%
          parse(file = NULL, n = NULL),
        xlab = "Predictors",
        ylab = "Relavtive importance",
        legend = imp %>% colnames(),
        args.legend = list(bty = "n",
                           x = "topright",
                           inset = c(0, -0.2)))
text(nrow(imp) + 0.0, 0.5, 
     paste("R squared = ", summary(lm_summ)$r.squared %>%
             round(3), sep = ""))

par(xpd=NA)
# box(lwd=0.5)
panel.let('b',font=1,shift=c(-0.09,-0.15), cex = 2)
par(xpd=F)

dev.off()


#######################################
library(tactile)

colnames(summ)[colnames(summ) == "q_50"] <- "Median"

i = "Median"

my.panel.bands <- function(x, y, upper, lower, fill, col,
                           subscripts, ..., font, fontface)
{
  upper <- upper[subscripts]
  lower <- lower[subscripts]
  panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                col = fill, border = FALSE,
                ...)
}

lm_5km <- lm(eval(parse(text = paste("beta~", i, sep = ""))), data = summ %>%
               scale() %>%
               as.data.frame())
lm_coeff <- summary(lm_5km)$coefficient
lm_r <- summary(lm_5km)$r.squared

round(lm_coeff[2,4], 3)
p_text <- "p-value < 0.01"

p_text <- paste("Coef = ",
                round(lm_coeff[2,1], 3),
                "\n",
                "R sqr = ", 
                round(lm_r, 3), 
                "\n",
                p_text, 
                sep = "")

obj1 <- xyplot(beta ~ ele, data = summ, type = "l" , lwd=2, col="steelblue",
               upper = summ$Max, lower = summ$Min,
               scales = list(tck = 0), xlab = "Elevation (m)",
               ylab = "Beta diversity",
               panel = function(...) {
                 panel.ci(..., alpha = 0.15, grid = F)
                 panel.xyplot(...)
               }
)
plot(obj1)


obj2 <- xyplot(eval(parse(text = paste(i, " ~ ele",  sep = ""))), 
               data = summ, type = "l", lwd=2, col="orange",
               scales = list(tck = 0), xlab = "Elevation (m)",
               ylab = ifelse(i == "PC",
                             i,
                             parse(text = paste("Cost~Distance[", gsub("q_", "Q[", i) , "]]" , sep = "")%>%
                                     gsub(pattern = "n]]", replacement = "n]"))))

fig <- doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE)



png("output/suppfig5A.png",width=7.95,height=6.9,unit="cm",res=300,pointsize=7)

grid.arrange(fig, ncol = 1,
             vp=viewport(width = 1, height=1, just = 0.5, gp = gpar(fontsize = 7, lwd = 1, cex = 0.5)))
KeyA<-list(space="right",
           lines=list(lwd = 2, col = c("steelblue", "orange")),
           text=list(c("Beta diveristy\n(w/SE)","Cost distance")), padding.text=5)
KeyB<-list(space="right",
           text=list('a'), padding.text=5)
KeyC<-list(space="right",
           text=list(p_text), padding.text=5)
draw.key(KeyA, draw = TRUE, vp = viewport(.25, .72, gp = gpar(fontsize = 5, lwd = 1)))
draw.key(KeyB, draw = TRUE, vp = viewport(0.05, .95, gp = gpar(fontsize = 15, lwd = 1)))
draw.key(KeyC, draw = TRUE, vp = viewport(.25, .84, gp = gpar(fontsize = 5, lwd = 1)))

dev.off()