distritos2 <- st_read("distritos_mapa.shp")


test2 <-
  distritos[test
    , on = 'coddistful'
    , .(t_mean := i.t_mean, p_y := i.p_y)]


sel_test <- val_distritos[distrito == "Liberia" &
                            canton == "Liberia" &
                            Scenario == "rpc45" & 
                            Model == "ccsm4_r1i1p1" &
                            Year >= 2030 & 
                            Year <= 2060][1, .(distrito, canton, coddistful)]

test <- val_distritos[
  Model == "ccsm4_r1i1p1" &
    Scenario == "rpc45" & 
    Year >= 2030 & Year <= 2060
  , .(t_mean = mean(t_mean_y), p_y = mean(p_y))
  , by = coddistful]

mapa_datos2 <- distritos2 %>% left_join(test, by = "coddistful")

mapa_sel_distrito <- mapa_datos2 %>% filter(coddistful == sel_test$coddistful)
mapa_sel_distrito2 <- mapa_datos2[coddistful == sel_test$coddistful] #seguro porque mapa_datos2 no es data.table




mapview(mapa_datos2, 
        zcol = "t_mean", 
        alpha.regions = 0.4,
        legend = T, 
        map.types = "CartoDB.Positron", 
        layer.name = "Grados C",
#        label , 
        popup = NULL)


pal <- colorNumeric(
  palette = "Oranges",
  domain = val_distritos$tmean)

m <- leaflet() %>% 
  addTiles() %>%
  addPolygons(
    data = mapa_datos2,
    fillColor = ~pal(p_y),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>% 
  setView(lng=-85.186, lat=10.451, zoom = 8)

m

m %>% addPolylines(
  data = mapa_sel_distrito,
  stroke = TRUE, 
#  fillOpacity = 0,
  weight = 4,
  color = "black"
)
