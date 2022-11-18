library(tidyverse)
library(network)
library(sna)

dat <- readxl::read_xlsx(path = "data/tweet_igloolik_030322.xlsx") 
dat

net <- 
    dat |> 
    filter(!is.na(in_reply_to_user_id)) |> 
    select(author_id, in_reply_to_user_id) |> 
    group_by(author_id, in_reply_to_user_id) |> 
    add_count() |> ungroup() |> unique() |> 
    network(directed = TRUE, loops = TRUE, matrix.type = "edgelist", ignore.eval = FALSE)

net

plot(net)

# retweet dataframe
rdf <- readxl::read_xlsx(path = "data/reply_igloolik_070322.xlsx") 
rdf


dat <- dat |> 
    left_join(rdf |> rename(author2_id = author_id))

links <- bind_rows(
    dat |> select(author_id, author2_id = in_reply_to_user_id),
    dat |> select(author_id, author2_id)) |> 
    filter(!is.na(author2_id))

net2 <- links |> 
    group_by(author_id, author2_id) |> 
    add_count() |> ungroup() |> 
    unique() |> 
    network(directed = FALSE, loops = TRUE, matrix.type = "edgelist", ignore.eval = FALSE)

plot(net2)


## bajar twits
## 