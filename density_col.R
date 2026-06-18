# 1. PACKAGES

pacman::p_load(
    arcgis,
    geodata,
    sf,
    dots,
    tidyverse,
    elevatr, terra,
    rayshader
)

library(jsonlite)


# 2. POPULATION DATA

url <-"https://microdatos.dane.gov.co/index.php/metadata/export/643/json"

data <- fromJSON(url)


#######

library(rvest)

url <- "https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion"
page <- read_html(url)
links <- page |>
    html_elements("a") |>
    html_attr("href")

excel_links <- links[grepl("\\.xlsx?$", links)]


library(readxl)

temp <- tempfile(fileext = ".xlsx")

download.file(
    paste0("https://www.dane.gov.co", excel_links[8]),
    temp,
    mode = "wb"
)

sheets <- excel_sheets(temp)

data_list <- lapply(sheets, function(x) {
    read_excel(temp, sheet = x)
})

names(data_list) <- sheets

data <- data_list[["PobDepartamentalxÁrea"]]

colnames(data) <- as.character(data[5, ])
data <- data[-5, ]

#######
year <- "2026" ### Change from 2018 to 2050 projection


filtered_data <- data %>%
    filter(
        .data$AÑO == year,
        .data$`ÁREA GEOGRÁFICA` == "Total"
    )

data_org <- filtered_data %>%
    arrange(DPNOM)
names(data_org)[names(data_org) == "DPNOM"] <- "NAME_1"
data_san <- data_org[4, ] ##### Extarct infro form San Andres
data_org <- data_org[-4, ]

# 3. Subnational Boundaries

country_admin1_sf <- geodata::gadm(
    country = "COL", ## Change by country
    level = 1,
    path = getwd()
) |>
    sf::st_as_sf() |>
    sf::st_cast(
        "MULTIPOLYGON"
    )

country_admin1_sf_san <- country_admin1_sf[27, ] ##### Extarct infro form San Andres
country_admin1_sf <-country_admin1_sf[-27, ]

#4. MERGE BOUDARIES AND POPULATIOS DATA

crs <- "+proj=tmerc +lat_0=4.59904722222222 +lon_0=-68.0809166666667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=intl +towgs84=221.899,274.136,-397.554,1.361573e-05,-2.174431e-06,-1.36241e-05,-2.199943 +units=m +no_defs +type=crs"

country_admin1_population <- dplyr::left_join(
    country_admin1_sf,
    data_org,
    by = "NAME_1"
)

country_admin1_population$Total <- data_org$TOTAL
country_admin1_population$Total <- as.numeric(country_admin1_population$Total)

country_admin1_population <- country_admin1_population |>
    sf::st_transform(crs = crs)



# 5. Calculate dot density

population_dots <- dots::dots_points(
    shp = country_admin1_population,
    col = "Total",
    engine = engine_sf_random,
    divisor = 70000
)

# 6. 2D dot desity map

p <- ggplot() +
    geom_sf(
        data= country_admin1_population,
        fill = "#153041",
        color = "#204863",
        linewidth = .5
    ) +
    geom_sf(
        data = population_dots,
        color = "#ffd301",
        size = .1
    ) +
    coord_sf(crs=crs) +
    theme_void()

print(p)


# 7. Digital Elevation Model

dem <- elevatr::get_elev_raster(
    locations = country_admin1_sf,
    z = 7,
    clip = "locations"
)

dem_reproj <- dem |>
    terra::rast() |>
    terra::project(crs)

dem_matrix <- rayshader::raster_to_matrix(
    dem_reproj
)


# 8. render boundary

dem_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            "white"
        )(16)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = country_admin1_population,
            palette = "#153041",
            linecolor = "#3D8DBF",
            linewidth = 5,
            extent = dem_reproj,
            heightmap = dem_matrix
        ), alphalayer = 1
    ) |>
    rayshader::plot_3d(
        dem_matrix,
        zscale = 50,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        windowsize = c(600, 600),
        phi = 89,
        zoom = .65,
        theta = 0
    )

rayshader::render_camera(
    zoom = .85
)

# 9. Render Points

coords <- sf::st_coordinates(
    population_dots
)

long <- coords[, "X"]
lat <- coords[,"Y"]

altitude <- terra::extract(
    x = dem_reproj,
    y = terra::vect(
        population_dots
    ),
    fun =min,
    na.rm =TRUE
)

altitude <- altitude[,2]

rayshader::render_points(
    lat = lat,
    long = long,
    altitude = altitude,
    extent = dem_reproj,
    heightmap = dem_matrix,
    zscale = 20,
    size = 3,
    color = "#ffd301"
)


# 10. Render object

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"

hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "3d-dot-density-austria.png",
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    rotate_env =90,
    interactive = FALSE,
    width = 4000,
    height = 4000,
    point_material = rayrender::glossy,
    point_material_args = list(
        color = "#ffd301",
        gloss = .4,
        reflectance = 0.1
    ),
    point_radius = 3
)
