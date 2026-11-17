gg_agbpools <- function(x, title) {
  gg <- ggplot() +
    tidyterra::geom_spatraster(data = x) +
    viridis::scale_fill_viridis(na.value = "transparent") +
    ggtitle(title) + theme_bw()
}

gg_landscapesummary <- function(x, 
                                pools = c("merch", "foliage", "other"),
                                colors = RColorBrewer::brewer.pal(3, 'Set1')) {
  d <- as.data.table(x) |>
    melt(id.vars = "year", variable.name = "pool", value.name = "biomass")
  d$pool <- factor(d$pool, levels= rev(pools))
  gg <- ggplot(d) + 
    geom_area(aes(x = year, fill = pool, y= biomass), position = position_stack()) +
    scale_fill_manual(values = colors) +
    ggtitle("AGB by pool across pixels") +
    labs(x = "Year", y = "Biomass") +
    theme_bw()
  return(gg)
}

gg_speciessummary <- function(x) {
  d <- as.data.table(x) |>
    melt(id.vars = c('speciesCode', "year"), variable.name = "pool", value.name = "biomass")
  gg <- ggplot(d) + 
    geom_area(aes(x = year, fill = speciesCode, y= biomass)) + 
    #scale_fill_manual(values = colors) +
    facet_grid(pool~., scales = "free_y") +
    ggtitle("AGB by pool and species across pixels") +
    labs(x = "Year", y = "Biomass") +
    theme_bw()
  return(gg)
}

gg_yieldCurves <- function(x, title){
  ggplot(x, aes(age, biomass, color = speciesCode)) + geom_line() + theme_bw() +
    #scale_color_manual(values = colors) +
    facet_wrap(~yieldTableIndex) +
    labs(title = title, x = "Age", y = 'AGB (tonnes/ha)', color = "Species") +
    theme_bw()
}

gg_yieldCurvesPools <- function(x, title) {
  ggplot(x, aes(age, B, fill = speciesCode)) +
    geom_area(position = position_stack()) +
    #scale_fill_manual(values = colors) +
    theme_bw() +
    facet_grid(yieldTableIndex ~ pool) +
    ggtitle(title) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank())
}
