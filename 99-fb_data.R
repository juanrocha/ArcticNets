library(tidyverse)
library(sf)
library(tictoc)
library(patchwork)

world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
                 fill = "#f9f9f9", size = 0.1) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() + theme_void(base_size = 6)
world + geom_hline(yintercept = 60)

#### GADM1 data for the world ####
## Prepare GADM data first. You need the codes to match with network places
tic()
gamdat <- read_sf("/Users/juanrocha/Documents/Projects/DATA/GADM_maps/gadm28.shp/gadm28.shp")
toc() #20s, 1.5GB

tic()
gamdat <- gamdat |> 
    mutate(center = st_centroid(geometry))
toc() #119.286 sec elapsed

tic()
arctic_gam <- gamdat |> 
    ## computer crashes without selecting one column, 
    ## the problem is memory mngt
    select(center) |> 
    mutate(center = as.character(center)) |> 
    mutate(center = str_remove_all(center, "c|\\(|\\)")) |> 
    separate(center, into = c("lon", "lat"), sep = ", ") |> 
    mutate(across(lon:lat, as.numeric)) |> 
    filter(lat > 60)
toc() # 12s, 75MB

tic()
arctic_gam <- arctic_gam |> 
    st_join(gamdat)
toc() # 200s, 851MB
rm(gamdat) # clean up memory
gc()

# save(arctic_gam, file = "data/arctic_gam.RData")
load("data/arctic_gam.RData")

gam_codes <- arctic_gam |> 
    as.data.frame() |> 
    mutate(gad_code = str_c(ISO, ID_1, "_", ID_2)) |> 
    pull(gad_code) |> 
    unique()



#### NUTS3 data for Europe ####
nuts <- read_sf("/Users/juanrocha/Documents/Projects/DATA/EuroStats_NUTS/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")

nuts |> skimr::skim()

nuts <- nuts |> 
    st_transform(4326) |> 
    mutate(center = st_centroid(geometry)) |> 
    mutate(center = as.character(center)) |> 
    mutate(center = str_remove_all(center, "c|\\(|\\)")) |> 
    separate(center, into = c("lon", "lat"), sep = ", ") |> 
    mutate(across(lon:lat, as.numeric)) |> 
    filter(lat > 60)

plot(st_geometry(nuts))

nut_codes <- nuts |> 
    as.data.frame() |> 
    filter(LEVL_CODE == 3) |> 
    pull(NUTS_ID)

#### USA: Alaska ####
## get the FIPS codes from https://www2.census.gov/geo/docs/reference/codes/files/st02_ak_cou.txt
ak <- read_csv(
    file = "https://www2.census.gov/geo/docs/reference/codes/files/st02_ak_cou.txt",
    col_names = c("state", "state_fips_code", "county_fips_code", "county_name", "class_fips"))

ak_codes <- ak |> 
    mutate(fb_code = str_c("USA", state_fips_code, county_fips_code)) |> 
    pull(fb_code)

#### Facebook data ####
tic()
dat <- read_tsv(file = "/Users/juanrocha/Documents/Projects/DATA/Facebook_social_connectedness_index/gadm1_nuts3_counties-gadm1_nuts3_counties - FB Social Connectedness Index - October 2021.tsv")
toc() # 14s
dat # 63M obs, 1.4GB in RAM

## get codes examples:
dat |> filter(str_detect(user_loc, "GL")) |> 
    pull(user_loc) |> 
    unique() # Russia does not exist in the dataset
# Greenland and Faroe Islands only exist at first level admin unit

places <- c(ak_codes, nut_codes, gam_codes, "GRL", "FRO")

# dat |> # head(n=100) |> print(n=100)
#     filter(str_detect(user_loc, "USA02")) # Alaska
#     # pull(user_loc) |>
#     # unique()


## filter only Arctic places:
tic()
dat <- dat |> 
    filter(user_loc %in% places & fr_loc %in% places)
toc() #5sec, object reduced to 57KB :)

## dat now has users only in the Arctic but with connections elsewhere. One can 
## use the degree on this network as part of the social capital to the non-Arctic
## regions. But the research question is really about the Arctic problems, so info
## flow within the Arctic.

dat |> skimr::skim()

a <- ggplot(dat, aes(scaled_sci)) +
    geom_density() +
    geom_rug(aes(color = scaled_sci), show.legend = FALSE) +
    scale_color_viridis_c(trans = "log10") +
    scale_x_log10(labels = scales::label_log(base=10)) + 
    labs(x = "Scaled Social Connectedness Index", 
         caption = "Data source: Facebook", y = "Density", tag = "A")+
    theme_light(base_size = 6)

b <- ggplot(dat, aes(fr_loc, user_loc )) +
    geom_tile(aes(fill = scaled_sci), show.legend = FALSE) +
    scale_fill_viridis_c(
        "Scaled SCI",trans = "log10", 
        guide = guide_colorbar(barwidth = unit(3, "mm"), barheight = unit(4,"cm"))) +
    labs(y = "User location", x = "Friend location", tag = "B") +
    theme_light(base_size = 6) +
    theme(axis.text.x = element_text(angle = 90), axis.text = element_text(size = 3))

ggsave(
    filename = "fb_arctic.png", width = 7, height = 4,
    path = "figs/", device = "png", dpi = 400, bg = "white",
    plot = a+b
)
a + b


save(dat, places, file = "data/fb_reduced.RData")



#### Left overs ####
gamdat |> names()
alaska <- gamdat |> 
    filter(ISO == "USA", NAME_1 == "Alaska")  

alaska |> select(starts_with("HAS"), "NAME_2", "CCA_2", "TYPE_2", "VALIDFR_2")
rm(gamdat)
