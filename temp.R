distritos2 <- st_read("distritos_mapa.json")


test2 <-
  distritos[test
    , on = 'coddistful'
    , .(t_mean := i.t_mean, p_y := i.p_y)]



test <- val_distritos[
  Model == "ccsm4_r1i1p1" &
    Scenario == "rpc45" & 
    Year >= 2030 & Year <= 2060
  , .(t_mean = mean(t_mean_y), p_y = mean(p_y))
  , by = coddistful]

mapa_datos2 <- distritos %>% left_join(test, by = "coddistful")

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
  domain = mapa_datos2$tmean)

leaflet(mapa_datos2) %>% 
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(t_mean),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>% 
  setView(lng=-85.186, lat=10.451, zoom = 8)
