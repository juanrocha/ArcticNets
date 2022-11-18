library(tidyverse)
library(statnet)
library(here)
library(fs)
library(sf)
library(ggnetwork)
library(glue)
library(lobstr)
library(tictoc)
#### Import data ####

# J221109: Working for one place, make it work for all at the same time
# 
fls <- dir_ls("data/MARAT Twitter/", recurse = TRUE)

## I need to add the eval(sym()) sequence so it does not store the name chr of the
## object, but the object itself.
tic()
twts <- str_subset(fls, "tweet_") |> 
    map(load, envir = parent.frame()) |> 
    map(sym) |> 
    map(eval)
toc() #4s
places <- str_subset(fls, "tweet_") |> 
    str_remove("data/MARAT Twitter/") |> 
    str_remove("\\.RData") |> 
    str_remove("/tweet")

twts <- map2(.x = twts, .y = places, 
         function(x,y) {x$place <- y; return(x)}
         )

## clean up work space, all tweets are now in twts
rm(list = ls()[str_detect(ls(), "tweet_")]) # finally!!
gc()

## this does not load all
safe_load <- safely(load)
tic()
rplys <- str_subset(fls, "replies_") |> 
    map(safe_load, envir = parent.frame())
toc() # 16s
# with this I can recover the error
rpl_df <- tibble(
    file = str_subset(fls, "replies_"),
    object = (transpose(rplys)$result)) |> 
    unnest(object, keep_empty = TRUE) 
print(rpl_df, n = 40) ## No errors!!
#this is the problematic one
#load("data/MARAT Twitter/Norway/replies_hammerfest_df.RData") 

rplys <- rpl_df |> 
    filter(!is.na(object)) |> 
    pull(object) |> 
    map(sym) |> 
    map(eval)

places2 <- rpl_df |> filter(!is.na(object)) |> pull(file) |> 
    #str_subset(fls, "replies_") |> 
    str_remove("data/MARAT Twitter/") |> 
    str_remove("\\.RData") |> 
    str_remove("/replies") |> 
    str_remove("_df")

places[!places %in% places2]
places2[!places2 %in% places]

rplys <- map2(.x = rplys, .y = places2, 
             function(x,y) {x$place <- y; return(x)}
)


twts <- bind_rows(twts) |> 
    as_tibble() # 466MB

rplys <- bind_rows(rplys) |> 
    as_tibble () #733 MB

## clean up work space, all tweets are now in twts
rm(list = ls()[str_detect(ls(), "replies_")]) # finally!!
gc()

## Visualizations

ggplot(map_data("world") |> rename(lon = long), aes(x = lon, y = lat)) +
    geom_path(aes(group = group), size = 0.1, color = "grey50") +
    coord_map(projection = "orthographic", orientation = c(90,0,0)) +
    ylim(45,90) +
    geom_point(data = twts |> 
                   unnest(geo) |> 
                   unnest(coordinates) |> 
                   mutate(coordinates = as.character(coordinates)) |> 
                   mutate(coordinates = str_remove_all(coordinates, pattern = "c\\(|\\)")) |> 
                   separate(col = coordinates, into = c("lon", "lat"), sep = ", ") |> 
                   mutate(across(.cols=lon:lat, .fns = as.numeric)) |> 
                   mutate(created_at = lubridate::as_date(created_at)),
               aes(lon, lat, color = created_at), size = 0.5) +
    labs(title = "Twitter data", 
         subtitle = glue({nrow(twts)}, "tweets from", {twts |> pull(place) |> unique() |> length()}, "communities", .sep = " "), 
         caption = "Data source: Twitter Academic API") +
    scale_color_viridis_c(option = "D", trans = "date") + 
    guides(color = guide_colorbar(title = "Date", title.position = "top",
                                  barwidth = unit(4,"cm"), barheight = unit(0.25, "cm"),
                                  direction = "horizontal")) +
    theme_void(base_size = 8) + theme(legend.position = c(0.2, 0.1), legend.text = element_text(size = 6))

ggsave(filename = "twits_map.png", plot = last_plot(), device = "png", path = "figs/",
       width = 4, height = 4, dpi = 400)

twts |> 
    filter(place == "Sweden_Vilhelmina" ) |> 
    mutate(created_at = lubridate::as_date(created_at)) |> 
    group_by(created_at) |> 
    summarize(N = n()) |> 
    ggplot(aes(created_at, N)) +
    geom_line()

places

# this is the chunk used to extract coordinates (used above on the graph):
# twts |> 
#     select(geo) |> 
#     unnest(geo) |> 
#     unnest(coordinates) |> 
#     mutate(coordinates = as.character(coordinates)) |> 
#     mutate(coordinates = str_remove_all(coordinates, pattern = "c\\(|\\)")) |> 
#     separate(col = coordinates, into = c("lon", "lat"), sep = ", ") |> 
#     mutate(across(.cols=lon:lat, .fns = as.numeric)) |> 
#     #mutate(coords = st_point(x = c(lon, lat)))
#     mutate(created_at = lubridate::as_date(created_at))

#### Networks ####
tic()
net <- bind_rows(
    twts |> 
        select(author_id, in_reply_to_user_id, place) |> 
        filter(!is.na(in_reply_to_user_id)) |> 
        group_by(author_id, in_reply_to_user_id, place) |> 
        summarize(N = n()) |> 
        ungroup(),
    rplys |> 
        #some replies have NA on author_id_Reply, but exist on author_id
        mutate(author_id_Reply = case_when(
            is.na(author_id_Reply) ~ author_id,
            TRUE ~ author_id_Reply
        )) |>
        select(author_id = author_id_Reply, in_reply_to_user_id, place) |> 
        group_by(author_id, in_reply_to_user_id, place) |> 
        summarize(N = n()) |> 
        ungroup() 
    ) |> 
    group_by(author_id, in_reply_to_user_id, place) |> 
    summarize(total = sum(N)) |> 
    group_by(place) |> 
    group_split() |> 
    map(network, directed = TRUE, matrix.type = "edgelist",
        ignore.eval=FALSE, loops = TRUE)
    # network(directed = TRUE, matrix.type = "edgelist", ignore.eval=FALSE, loops = TRUE)
toc() # 25s
 
length(net) # 34 networks

tic()
net_stats <- tibble(
    density = map_dbl(net, network.density),
    nodes = map_dbl(net, network.size),
    edges = map_dbl(net, network.edgecount),
    place = map_chr(net, function(x) get.edge.attribute(x, "place")[1]),
    central_degree = map_dbl(net, centralization, degree),
    central_bet = map_dbl(net, centralization, betweenness),
    connected = map_dbl(net, connectedness),
    efficiency = map_dbl(net, efficiency),
    reciprocity = map_dbl(net, grecip, "edgewise"), # proportion of reciprocated edges
    hierarchy = map_dbl(net, hierarchy),
    transitivity = map_dbl(net, gtrans)
)
toc() #200s

net_stats |> 
    separate(place, into = c("country", "place"), sep = "_") |> 
    mutate(place = str_to_title(place)) |> 
    pull(place)

net_stats$out <- c(
    "L", #"Aklavik"
    "T", #"Capedorset"
    "L", #"Igloolik"
    "A", #"Ikiparjuk"
    "A", #"Iqualit"
    "L", #  "Kugluktuk" 
    "L",# "Newfoundland"  
    "A", # "Pangnirtung"  
    "A", #   "Ulukhaktok"
    "T", # "Barent"
    "A",# "Näätämo
    "L",# "Paamiut" 
    "L", # "Qaanaaq"
    "T", # "Sisimiut"
    "L", #  "Upernavick"
    "L", #  "Uummannaq"
    "T", #  "Husavik"
    "L", # "Siglufjörour
    "L", #  "Finmark"
    "L",# "Hammerfest"
    "A",# "Nesseby"
    "A",#  "Porsangu"
    "A",#  "Unjarga"
    "A",# "Varanger"
    "L", # "Kharatumul" 
    "L", # "Kyllakh"
    "L", #  "Teriberka"
    "L",#  "Tiksi"
    "L",# "Varnek"
    "A",#  "Yamalnenets" 
    "A",#  "Kiruna"
    "L",#   "Vilhelmina" 
    "A",#  "Savoonga
    "T"# "Westernalaska"
)

net_stats <- net_stats |> 
    mutate(out = as_factor(out)) |> 
    separate(place, into = c("country", "place"), sep = "_") |> 
    mutate(place = str_to_title(place))

plot.network(
    net[[2]], edge.lwd = 0.05, edge.col = alpha("grey50", 0.75),
    vertex.border = 0, vertex.col = alpha("blue", 0.75), vertex.lwd = 0.01,
    main = unique(net[[2]] %e% "place") |> str_split(, pattern = "_"), cex.main = 0.8)

quartz.save(file="figs/twitter_net2.png", 
            type = "png", dpi = 300, width = 4, height = 4, pointsize = 6)



net_stats |> skimr::skim()

net_stats |> 
    select(where(is.numeric)) |> 
    GGally::ggpairs()

ggplot(net_stats, aes(out, density)) +
    geom_boxplot(aes(fill = out, color = out), alpha = 0.5, size = 0.3) +
    geom_jitter()

net_stats |> 
    mutate(out2 = ifelse(out == "A" | out == "T", "R",'L')) |> 
    ggplot(aes(nodes, edges)) +
    geom_point(aes(color = country, shape = out)) +
    scale_shape("Output", guide = guide_legend(title.position = "top"))+
    ggrepel::geom_text_repel(
        aes(label = place), size = 1.5) +
    scale_color_brewer("Country",palette = "Set1", guide = guide_legend(title.position = "top")) +
    scale_x_log10() + scale_y_log10() +
    theme_light(base_size = 8) + theme(
        legend.position = c(0.35,0.8), legend.direction = "horizontal", 
        legend.text = element_text(size = 5), legend.title = element_text(size = 6)
    )

ggsave(filename = "net_sizes.png", path = "figs/", device = "png", width = 4, height = 4,
       dpi = 400)

 net_stats |> 
    pivot_longer(cols = where(is.numeric), values_to = "value", names_to = "stat") |> 
    ggplot(aes(out, value)) +
    geom_boxplot(aes(fill = out, color = out), alpha = 0.5, size = 0.3) +
    #geom_jitter(aes(shape = country)) +
    facet_wrap(~stat, scales = "free_y") +
    labs(title = "Some descriptive stats", x = "Output in QCA",
         subtitle = glue("{nrow(net_stats)} twitter community networks (retweets)"),
         caption = "Source = Twitter Academic API") +
    theme_light(base_size = 6) + 
    theme(legend.position = c(0.8, 0.1), legend.direction = "horizontal")

ggsave(filename = "twitter_netstats.png", path = "figs/", device = "png", width = 4,
       height = 3, dpi = 300)

net_stats |> 
    pull(out) |> 
    table()

# ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_edges(aes(color = N), size = 0.1) +
#     geom_nodes() +
#     theme_void()

ls() |> str_subset("tweet_") 


tweet_pa |> as_tibble() |> names()

tweet_pa |> 
    select(author_id, conversation_id, geo)

replies_paa |> #names()
    select(author_id_Reply, conversation_id, geo)

## the geo information in replies does not have coordinates, instead it has a place_id
## which is a json standard. Read more here on how to retrieve it
## https://developer.twitter.com/en/docs/twitter-api/v1/geo/place-information/api-reference/get-geo-id-place_id
## But I think one can also do with google maps or other services: https://developers.google.com/maps/documentation/javascript/examples/places-placeid-finder

## It seems it could be read as sf object and plot as points... check later:
tweet_pa$geo$coordinates$coordinates 
    


## combine with replies
str_subset(fls, "replies_") |> 
    map(load, envir = parent.frame())

## This are just lists with the replies from the API, each element is a dataframe
## that lists replies when available. The df.RData object has the same info already
## on one single object.
load("data/MARAT Twitter/Greenland/reply_paamiut_original.RData")


