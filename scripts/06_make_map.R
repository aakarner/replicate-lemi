source("R/utils.R")
check_packages(c("leaflet", "htmlwidgets"))
ensure_directories()

cfg <- read_config()
tracts <- sf::st_read("data/processed/lemi_study_tracts.gpkg", quiet = TRUE) |>
  dplyr::select(geoid)
scores <- readr::read_csv(
  "output/lemi_scores_corrected.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

mapped <- tracts |>
  dplyr::left_join(
    scores |>
      dplyr::select(
        geoid, name, lemi_pctl, health_wb_pctl, liv_work_pctl, acc_bel_pctl
      ),
    by = "geoid"
  )

stopifnot(!anyNA(mapped$lemi_pctl))

breaks <- as.numeric(unlist(cfg$map$breaks))
labels <- unname(unlist(cfg$map$labels))
colors <- unname(unlist(cfg$map$colors))
mapped$barrier_class <- cut(
  mapped$lemi_pctl,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = TRUE
)

plot <- ggplot2::ggplot(mapped) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = barrier_class),
    color = scales::alpha("black", 0.25),
    linewidth = 0.15
  ) +
  ggplot2::scale_fill_manual(values = stats::setNames(colors, labels), drop = FALSE) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "Austin Levers of Economic Mobility Index",
    subtitle = "Corrected replication of the documented formula | 2020 census tracts",
    fill = NULL,
    caption = "Higher scores indicate more favorable structural conditions."
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(color = "grey30"),
    legend.position = "right",
    plot.margin = ggplot2::margin(12, 12, 12, 12)
  )

ggplot2::ggsave(
  "output/lemi_map_corrected.png",
  plot,
  width = 10,
  height = 8,
  dpi = 220,
  bg = "white"
)

popup <- paste0(
  "<strong>", mapped$name, "</strong><br>",
  "LEMI: ", round(mapped$lemi_pctl, 1), "<br>",
  "Health & Wellbeing: ", round(mapped$health_wb_pctl, 1), "<br>",
  "Livelihood & Work: ", round(mapped$liv_work_pctl, 1), "<br>",
  "Access & Belonging: ", round(mapped$acc_bel_pctl, 1)
)

palette <- leaflet::colorBin(
  palette = colors,
  domain = mapped$lemi_pctl,
  bins = breaks,
  right = TRUE
)

interactive_map <- leaflet::leaflet(sf::st_transform(mapped, 4326)) |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
  leaflet::addPolygons(
    fillColor = ~palette(lemi_pctl),
    fillOpacity = 0.82,
    color = "#333333",
    weight = 0.4,
    opacity = 0.5,
    popup = popup,
    label = ~paste0(name, ": ", round(lemi_pctl, 1))
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    colors = colors,
    labels = labels,
    title = "Structural barriers",
    opacity = 0.9
  )

htmlwidgets::saveWidget(
  interactive_map,
  "output/lemi_map_corrected.html",
  selfcontained = TRUE
)

sf::st_write(
  mapped,
  "output/lemi_corrected.gpkg",
  layer = "lemi_corrected",
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Corrected static, interactive, and GeoPackage maps written to output/.")
