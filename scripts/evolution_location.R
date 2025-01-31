##
## location-based
##

pacman::p_load(dplyr, ggplot2, gganimate)

# set seed for reproducibility
seed_num <- 137
set.seed(seed_num)

# set population at start of simulation
start_pop <- 120

# create dataframe for initial population
population <- data.frame(id = c(0, seq(1:start_pop)),
                         type = "o",
                         parent_id = 0,
                         gen = c(0, rep(1, start_pop))) |>
  rowwise() |>
  mutate(age = sample(seq(1:20), 1),
         loc_y = runif(1, -5, 5),
         loc_x = runif(1, -5, 5))

# create copy of population
population_all <- population

# create variables for processing
population_count <- as.numeric(nrow(population))
top_id <- max(population$id)
year_count <- 0

# age at which reproduction happens
baby_agerange <- c(20, 40)

# create evenly spaced optimal locations for each mutation
L <- 50
R <- L / sqrt(3)
th <- 20 * pi / 180

a_x <- (R * cos(th + ((2 * pi)/3)))
a_y <- (R * sin(th + ((2 * pi)/3)))
b_x <- (R * cos(th))
b_y <- (R * sin(th))
c_x <- (R * cos(th + ((4 * pi)/3)))
c_y <- (R * sin(th + ((4 * pi)/3)))  
  
# set probabilities for the simulation
probabilities <- data.frame(type = c("o", "m", "x"),
                            baby_prob = c(0.22, 0.22, 0.23),
                            o_prob = c(0.96, 0.03, 0.01),
                            m_prob = c(0.08, 0.90, 0.02),
                            x_prob = c(0.01, 0.01, 0.98),
                            death_prob = c(0.02, 0.0198, 0.0195),
                            p_loc_y = c(a_y, b_y, c_y),
                            p_loc_x = c(a_x, b_x, c_x)
)

# set colours
o_col <- "#6773CD" # blue
m_col <- "#CD6C46" # dark red
x_col <- "#7AB159" # green

# create palette
pal <- c("o" = o_col,
         "m" = m_col,
         "x" = x_col)

# mutations by year
muts_by_year <- data.frame(year = 0,
                           no_mutation = population_count,
                           mutation = 0,
                           x_mutation = 0)

# number of iterations to run
num_it <- 3000

## list for iteration results
results_list <- vector("list", num_it)

# # create blank dataframe
# all_all <- data.frame()

# iterate through years - pause at any time - recommend over 500
for (i in 1:num_it) {
    
    # apply updates to the population
    pop_changes <- population |>
      left_join(probabilities, by = "type") |>
      mutate(dist_adj = sqrt(((loc_x - p_loc_x) ^ 2) + ((loc_y - p_loc_y) ^ 2)) / 3000, # effect on death probability when further from optimal location
             new_baby = ifelse(age < baby_agerange[1], 0,
                               ifelse(age > baby_agerange[2], 0,
                                      sample(c(1, 0), 1, prob = c(baby_prob, 1 - baby_prob)))),
             loc_x = loc_x + runif(1, -1, 1),
             loc_y = loc_y + runif(1, -1, 1),
             baby_loc_x = loc_x + runif(1, -2, 2),
             baby_loc_y = loc_y + runif(1, -2, 2),
             new_baby_type = sample(c("o", "m", "x"), 1, prob = c(o_prob, m_prob, x_prob)),
             death_prob = (ifelse(age > 70, death_prob * 10, death_prob) + dist_adj) 
             * (population_count / (start_pop + (year_count / 10))), # added to ensure population doesn't get too high
             death_prob = ifelse(death_prob > 1, 1, death_prob),
             death = sample(c(1, 0), 1, prob = c(death_prob, 1 - death_prob))
      )
    
    # create new baby lines
    new_babies <- pop_changes |>
      filter(new_baby == 1) |>
      mutate(parent_id = id,
                id = top_id + seq_len(n()),
                type = new_baby_type,
                age = 0,
                gen = gen + 1,
                loc_y = baby_loc_y,
                loc_x = baby_loc_x) |>
      select(parent_id, id, type, age, gen, loc_y, loc_x)
    
    # remove death lines & add babies
    population <- pop_changes |>
      filter(death == 0) |>
      select(id, type, parent_id, age, gen, loc_y, loc_x) |>
      mutate(age = age + 1) |>
      rbind(new_babies)
    
    # add year_count to df
    all_pop <- population
    all_pop$year <- year_count
    
    # add to results list
    results_list[[i]] <- all_pop
    
    # get population stats
    population_count <- as.numeric(nrow(population))
    top_id <- max(population$id)
    year_count <- year_count + 1
    orig_current <- nrow(population[population$type == "o",])
    mut_current <- nrow(population[population$type == "m",])
    x_current <- nrow(population[population$type == "x",])
    
    # create row for current iteration year
    new_year <- data.frame(year = year_count,
                           no_mutation = orig_current,
                           mutation = mut_current,
                           x_mutation = x_current)
    
    # add to mutation summary df
    muts_by_year <- rbind(muts_by_year, new_year)
    
    if (year_count %% 100 == 0) {
      cat("\r", year_count, "years complete       ")
      }
    
}

# combine all results
all_all <- bind_rows(results_list)

## plots

# population |>
#   ggplot(aes(x = loc_x, y = loc_y)) +
#   geom_point(aes(color = type), position = position_jitter(w = 0.5)) +
#   annotate(geom = "text", x = 1, y = 0, label = paste0(year_count, " years passed"), hjust = 0.5) +
#   geom_point(data = probabilities, aes(x = p_loc_x, y = p_loc_y, color = type), shape = 25, size = 6, stroke = 2) +
#   # annotate(geom = "text", x = 2, y = 80, label = paste0(orig_current, " with no mutation"), hjust = 1) +
#   # annotate(geom = "text", x = 2, y = 60, label = paste0(mut_current, " with mutation"), hjust = 1) +
#   # annotate(geom = "text", x = 2, y = 40, label = paste0(x_current, " with x mutation"), hjust = 1) +
#   scale_color_manual(values = pal) +
#   coord_equal()
# 
# muts_by_year |>
#   ggplot(aes(x = year)) +
#   geom_line(aes(y = mutation), color = m_col) +
#   geom_line(aes(y = no_mutation), color = o_col) +
#   geom_line(aes(y = x_mutation), color = x_col)

## animations

# static version of animated chart to design without having to re-animate every time
# all_all |>
#   ungroup() |>
#   # filter(year == 850) |>
#   ggplot(aes(x = loc_x, y = loc_y)) +
#   geom_point(aes(color = type), position = position_jitter(w = 0.5)) +
#   geom_point(data = probabilities, aes(x = p_loc_x, y = p_loc_y, color = type), shape = 25, size = 6, stroke = 2) +
#   #annotate(geom = "text", x = 1, y = 0, label = paste0(year_count, " years passed"), hjust = 0.5) +
#   # annotate(geom = "text", x = 10, y = -10, label = paste0(orig_current, " with no mutation"), hjust = 0) +
#   # annotate(geom = "text", x = 10, y = -13, label = paste0(mut_current, " with mutation"), hjust = 0) +
#   # annotate(geom = "text", x = 10, y = -16, label = paste0(x_current, " with x mutation"), hjust = 0) +
#   scale_color_manual(values = pal) +
#   coord_equal()

# Create a separate data frame with one row per year
year_labels <- all_all %>%
  distinct(year) %>%
  mutate(x = 1, y = 0, label = paste0(year, " years passed"))

evo_animation <- all_all |>
  # filter(year == 850) |> # used for creating a static chart to test redesigns on
  ggplot(aes(x = loc_x, y = loc_y)) +
  geom_point(aes(color = type), position = position_jitter(w = 0.5)) +
  geom_point(data = probabilities, aes(x = p_loc_x, y = p_loc_y, color = type), shape = 25, size = 6, stroke = 2) +
  # annotate(geom = "text", x = 1, y = 0, label = paste0(year_count, " years passed"), hjust = 0.5) +
  geom_text(data = year_labels, aes(x = x, y = y, label = label), hjust = 0.5, size = 6) +
  scale_color_manual(values = pal) +
  transition_time(year) +
  coord_equal() +
  theme_void()

evo_animation

anim_save(paste0("outputs/evolution_",seed_num, "_",max(all_all$year)+1,".gif"), evo_animation)

# see population levels oveer time
pop_animation <- muts_by_year |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = mutation), color = m_col) +
  geom_line(aes(y = no_mutation), color = o_col) +
  geom_line(aes(y = x_mutation), color = x_col) +
  labs(y = "population") +
  transition_reveal(along = year) +
  view_follow() +
  theme_minimal()

anim_save(paste0("outputs/population_",seed_num, "_",max(all_all$year)+1,".gif"), pop_animation)
