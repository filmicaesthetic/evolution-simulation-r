
# library
library(igraph)

# create data:
links <- data.frame(
  source=population_all$parent_id,
  target=population_all$id,
  importance=population_all$gen
)

pop_all_uniq <- population_all |> select(id, type) |> unique()

pop_all_uniq |>
  group_by(id) |>
  summarise(n = n()) |>
  filter(n > 1)

nodes <- data.frame(
  name=pop_all_uniq$id,
  carac=pop_all_uniq$type
)

# Turn it into igraph object
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 

# Make a palette of 3 colors
library(RColorBrewer)
coul  <- brewer.pal(3, "Set1") 

# Create a vector of color
my_color <- coul[as.numeric(as.factor(V(network)$carac))]

# Make the plot
plot(network, vertex.color=my_color)

# # Add a legend
# legend("bottomleft", legend=levels(as.factor(V(network)$carac))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))
