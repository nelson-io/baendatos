#import libs
library(tidyverse)
library(janitor)
library(sf)
library(downloader)
library(ggthemes)
library(cartogram)
library(transformr)
library(gganimate)
#import data

eah <- map_df(2019:2012, ~ read_csv(paste0('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/direccion-general-de-estadisticas-y-censos/encuesta-anual-hogares/encuesta-anual-hogares-',
                                 .x, '.csv'), locale = locale(encoding = 'Windows-1252')) %>% 
             mutate(year = .x)) %>% 
  clean_names()



eah_summarised <- eah %>% 
  filter(estado_ocupacional == 'Ocupado',
         calidad_ingresos_lab == 'Tuvo ingresos y declara monto',
         !is.na(ingresos_totales),
         ingresos_totales > 0) %>% 
  group_by(year, comuna, sexo) %>% 
  summarise(ingresos_totales_lab_median = median(ingreso_total_lab) )%>% 
  ungroup() %>% 
  pivot_wider(names_from = sexo, values_from = ingresos_totales_lab_median) %>% 
  mutate(diferencial_ingresos_medianos = 1- Mujer/Varon)


#import comunas


url = 'https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas-zip.zip'
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./data")
file.remove('dataset.zip')

comunas <- st_read('data/comunas.shp')


#join data

joined_Data <- comunas %>% 
  left_join(eah_summarised, by = c('COMUNAS' = 'comuna'))



#plot_data

cartograms_list <- map(joined_Data$year %>% unique(), ~ cartogram_cont(joined_Data %>% filter( year == .x),"diferencial_ingresos_medianos", itermax=20))
cartogram_df = do.call('rbind', cartograms_list)
cartogram_df <- cbind(cartogram_df, st_coordinates(st_centroid(cartogram_df)))


p <- ggplot(cartogram_df ) + 
  geom_sf(
    aes(fill = diferencial_ingresos_medianos)
    )+
  geom_text(aes(x = X, y = Y, label = paste0(round(100*diferencial_ingresos_medianos), '%')), color = 'white', size = 7)+
  scale_fill_gradient(
    low = "#C2C2C2",
    high = "#FF0000",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill", labels = scales::percent)+
  labs(fill = "Diferencial de Ingresos Medianos")+
  theme_map()+ 
  theme(legend.position = c(.5, 0.05),
                     legend.direction = "horizontal",
                     plot.title = element_text(size=50),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.key.size = unit(1,'cm')) 

anim <- p+ transition_states(year, transition_length = 2, state_length = 5) + ggtitle('Año {closest_state}')

# animate(anim,
#         renderer = ffmpeg_renderer(options = list(pix_fmt = "yuv420p")),
#         width = 800, height = 800, duration = 30, nframes = 300)

animate(anim,
        renderer = file_renderer(dir = './plots', prefix = 'gganim_plot', overwrite = T),
        width = 800, height = 800, duration = 30, nframes = 300)

# anim_save('anim.mp4')
