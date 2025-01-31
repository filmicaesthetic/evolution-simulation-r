

pacman::p_load(dplyr, ggplot2)

population <- data.frame(id = c(0, seq(1:100)),
                         type = "o",
                         parent_id = 0,
                         gen = c(0, rep(1, 100))) |>
  rowwise() |>
  mutate(age = sample(seq(1:101), 1))

population_all <- population |> select(-age)

population_count <- as.numeric(nrow(population))
top_id <- max(population$id)
year_count <- 0

baby_agerange <- c(20, 40)

probabilities <- data.frame(type = c("o", "m", "x"),
                            baby_prob = c(0.12, 0.12, 0.5),
                            o_prob = c(0.996, 0, 0.1),
                            m_prob = c(0.0039, 0.999, 0.8),
                            x_prob = c(0.0001, 0.001, 0.1),
                            death_prob = c(0.03, 0.025, 0.0001)
                            )

pal <- c("o" = "blue",
         "m" = "red",
         "x" = "purple")

muts_by_year <- data.frame(year = 0,
                           no_mutation = population_count,
                           mutation = 0,
                           x_mutation = 0)

for (i in 1:600) {
  
  population_all <- rbind(population_all, population |> select(-age)) |> unique()
    
    
    pop_changes <- population |>
      left_join(probabilities, by = "type") |>
      mutate(new_baby = ifelse(age < baby_agerange[1], 0,
                               ifelse(age > baby_agerange[2], 0,
                                      sample(c(1, 0), 1, prob = c(baby_prob, 1 - baby_prob)))),
             #new_baby_mut = ifelse(new_baby == 0, 0, sample(c(1, 0), 1, prob = c(baby_mut_prob, 1 - baby_mut_prob))),
             #new_baby_x = ifelse(new_baby_mut == 1, sample(c(1, 0), 1, prob = c(0.01, 0.99)), 0),
             new_baby_type = sample(c("o", "m", "x"), 1, prob = c(probabilities$o_prob[probabilities$type == type], probabilities$m_prob[probabilities$type == type], probabilities$x_prob[probabilities$type == type])),
             death_prob = ifelse(age > 70, death_prob * 10, death_prob) * (population_count / (100 + (year_count / 100))),
             death = sample(c(1, 0), 1, prob = c(death_prob, 1 - death_prob))
      )
    
    # create new baby lines
    new_babies <- pop_changes |>
      filter(new_baby == 1) |>
      ungroup() |>
      summarise(parent_id = id,
                id = top_id + row_number(),
                #type = ifelse(new_baby_mut == 1, ifelse(new_baby_x == 1, "x", "m"), "o"),
                type = new_baby_type,
                age = 0,
                gen = gen + 1)
    
    # remove death lines & add babies
    population <- pop_changes |>
      filter(death == 0) |>
      select(id, type, parent_id, age, gen) |>
      mutate(age = age + 1) |>
      rbind(new_babies)
    
    population_count <- as.numeric(nrow(population))
    top_id <- max(population$id)
    year_count <- year_count + 1
    orig_current <- nrow(population[population$type == "o",])
    mut_current <- nrow(population[population$type == "m",])
    x_current <- nrow(population[population$type == "x",])
    
    new_year <- data.frame(year = year_count,
                           no_mutation = orig_current,
                           mutation = mut_current,
                           x_mutation = x_current)
    
    muts_by_year <- rbind(muts_by_year, new_year)
    
}


population |>
  ggplot(aes(x = 1, y = age)) +
  geom_point(aes(color = type), position = position_jitter(w = 0.5)) +
  annotate(geom = "text", x = 2, y = 100, label = paste0(year_count, " years passed"), hjust = 1) +
  annotate(geom = "text", x = 2, y = 80, label = paste0(orig_current, " with no mutation"), hjust = 1) +
  annotate(geom = "text", x = 2, y = 60, label = paste0(mut_current, " with mutation"), hjust = 1) +
  annotate(geom = "text", x = 2, y = 40, label = paste0(x_current, " with x mutation"), hjust = 1) +
  scale_color_manual(values = pal)

muts_by_year |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = mutation), color = "red") +
  geom_line(aes(y = no_mutation), color = "blue") +
  geom_line(aes(y = x_mutation), color = "purple")

population_all



library(gganimate)  

muts_by_year |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = mutation), color = "red") +
  geom_line(aes(y = no_mutation), color = "blue") +
  geom_line(aes(y = x_mutation), color = "purple") +
  transition_reveal(along = year) +
  view_follow()



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
