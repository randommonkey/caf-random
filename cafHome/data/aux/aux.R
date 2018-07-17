dataCaf <- read_csv('data/clean/movilidad_latam_data.csv')
dicCaf <- read_csv('data/clean/movilidad_latam_dic_.csv') %>% drop_na(grupo)
dicCaf <- dicCaf %>% filter(ctypes == 'Num')
dataCaf <- dataCaf[, c('pais', 'ciudad',dicCaf$id)]
mapLam <- jsonlite::fromJSON("data/latin-america.json", simplifyVector = FALSE)
codigos <- read_csv('data/clean/codigos.csv')

ciudades <- unique(dataCaf$ciudad)

map(ciudades, function(idCiu){
df <- codigos[codigos$ciudad == idCiu,]
df <- df %>% dplyr::select(lon, lat)
paisSel <- codigos$ggch[codigos$ciudad == idCiu]
opts <-  list(titleLabel = "",
              subtitle = "",
              caption = "",
              reverse = FALSE,
              fillLabel = NULL,
              text = FALSE,
              text_size = 1.5,
              prop_text = 'all',
              leg_pos = "left",
              titleLeg = '',
              scale_point = 21,
              color_map = "white",
              color_point = '#a0e13a',
              alpha = 1,
              color_frontier = "white",
              Bcolor = 'transparent')
h <- geomagic::gg_bubble_GcdLonLat.(data = df, mapName = paisSel, opts = opts) + 
   theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
           ggsave(paste0("www/imgMp/", idCiu, ".png"), bg = "transparent", width = 25, height = 25) 

})
