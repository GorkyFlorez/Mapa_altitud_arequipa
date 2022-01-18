#-----------------------------------------------------------------------------------
#https://github.com/GorkyFlorez/Mapa_altitud_arequipa
library(ggplot2)
library(raster)
library(tidyverse)
library(elevatr)
library(sf)
library(sp)
#-----------------------------------------------------------------------------------
Peru_p             <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Arequipa           <- subset(Peru_p, NAME_1  == "Arequipa")
alt      <- getData('alt', country='Peru')  
Area_alt<- crop(alt,Arequipa)                           #   
Area_alt<- Area_alt<- mask(Area_alt, Arequipa)
dem.p          <-  rasterToPoints(Area_alt)
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6247)
cols <-c("#5F8141","#779F44","#93C649","#A9DD55","#CBD689","#ECE5B2","#E1C678","#978055","#45280E")
#-----------------------------------------------------------------------------------
Perr =ggplot() +
  geom_sf(data=Peru, color="white", fill="gray90", size=0.5)+
  geom_sf(data=Arequipa, fill="gray", color="gray")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4))

Are= ggplot() +
  geom_raster(data = df , aes(lon,lat, fill = alt), size =0.2)+
  geom_sf(data=Arequipa, color="white", fill=NA)+
  scale_fill_gradientn(colours = cols,
                       na.value = 'white',name="Elevacion\n(m.s.n.m)",breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6247]"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = "Mapa de elevacion de Arequipa",
       #subtitle  = "Distritos de arequipa",
       caption = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
       size = "f")+
  theme_bw()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")"))) +
  theme(panel.grid.major = element_line(color = gray(.7),linetype = "dashed", size = 0.2),
    axis.text = element_text(colour = "black", size = 8),
        axis.text.y  = element_text(angle = 90),
    plot.title = element_text(size = 21, hjust = 0.5, color = "#4e4d47", family="serif"),
    plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47"),
    plot.caption = element_text(size = 8, hjust = 0.95, color = "#4e4d47", family="serif"),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    legend.text =element_text(size=9, family="serif"),
    legend.title = element_text(size=9, family="serif"),
    legend.key.size = unit(0.3, "cm"), 
    legend.key.width = unit(0.9,"cm"),
    legend.position = c(0.92,0.17))+
  guides(fill = guide_legend())

Grafica =ggplot() +
  annotation_custom(ggplotGrob(Are), xmin = 0, xmax = 29, ymin = 0, ymax = 21) +
  annotation_custom(ggplotGrob(Perr), xmin = 1.5, xmax = 6.5, ymin = 3, ymax = 8) +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE)+
  theme_bw()+
  theme(plot.background = element_rect(colour = "black",size = 2.5))
#------------------------------------------------------------------------
ggsave(plot = Grafica  ,"Mapas/Altitud de arequipa.png", units = "cm", 
       width = 29,height = 21, dpi = 900)


