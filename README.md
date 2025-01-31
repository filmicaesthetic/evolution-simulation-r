## About

This project explores simulations of evolution in population through
mutations that affect reproduction and survival rate.

## Visuals

#### Location-based evolution simulation

One of the models creates an “optimal location” for each mutation, where
their survival rate is highest, and places the entire population with no
mutation at the centre of the 3 optimal locations. Each individual is
programmed to move randomly every year, with specified reproducing age,
probablity of reproduction, probability of death. Generally populations
form around each mutation’s optimal location over a few hundred
iterations.

These visuals were created with the evolution\_location.R script, and
shows 3 distinct populations with different birth, death & mutation
rates over 3,000 years.

![Location-based evolution progression showing crowds of 3 different
coloured dots seemingly moving toward their optimal location while
population fluctuates over 3,000 years](outputs/evolution_137_3000.gif)

![Animated line chart showing change in populations for each mutation
throughout the course of the simulation, starting off with 120 blue (no
mutation), red and green (2 different mutations) overtaking at about 400
years. Red plateaus and declines from around 1,000 years while green
continues to grow, ending up at around 800 by year
3000](outputs/population_137_3000.gif)
