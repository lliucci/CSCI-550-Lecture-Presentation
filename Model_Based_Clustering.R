library(mclust)
library(tidyverse)
library(MASS)

# Load in data
data("geyser")

geyser %>% head()

# Plot raw data and save
geyser_viz = ggplot(data = geyser, aes(x = waiting, y = duration)) +
    geom_point() +
    labs(x = "Waiting Time",
        y = "Duration of Geyser")

ggsave(geyser_viz,
    filename = "Figures/Geyzer_Viz.png",
    scale = 2,
    height = 800,
    width = 1200,
    units = 'px')

# Perform model based clustering
MClusters = Mclust(geyser)

Classes <- tibble(Classes = MClusters$classification)

geyser <- cbind(geyser, Classes)

geyser <- geyser %>%
  mutate(Classes = factor(Classes))

# Plot raw data, colored by cluster
geyser_classes_viz = ggplot(data = geyser, aes(x = waiting, 
                                        y = duration, 
                                        color = Classes)) +
    geom_point() +
    labs(x = "Waiting Time",
        y = "Duration of Geyser")

ggsave(geyser_classes_viz,
    filename = "Figures/Geyzer_Classes_Viz.png",
    scale = 2,
    height = 800,
    width = 1200,
    units = 'px')

# Add contours for likelihoods
means = MClusters$parameters$mean
cov = MClusters$parameters$variance$sigma

waiting_seq = seq(min(geyser$waiting), max(geyser$waiting), length = nrow(geyser))
duration_seq = seq(min(geyser$duration), max(geyser$duration), length = nrow(geyser))

grid_data = expand.grid(waiting_seq, duration_seq)

grid_data %>% head()

# Class 1
class_1_lhood = dmvnorm(data = grid_data,
                        mean = means[,1],
                        sigma = cov[,,1])
nc = sum((waiting_seq[2] - waiting_seq[1]) * (duration_seq[2] - duration_seq[1]) * class_1_lhood)
class_1_norm_lhood = class_1_lhood/nc
c1_lhood_data = bind_cols(grid_data, tibble(lhood = class_1_norm_lhood))

# Class 2
class_2_lhood = dmvnorm(data = grid_data,
                        mean = means[,2],
                        sigma = cov[,,2])
nc = sum((waiting_seq[2] - waiting_seq[1]) * (duration_seq[2] - duration_seq[1]) * class_2_lhood)
class_2_norm_lhood = class_2_lhood/nc
c2_lhood_data = bind_cols(grid_data, tibble(lhood = class_2_norm_lhood))

# Class 3
class_3_lhood = dmvnorm(data = grid_data,
                        mean = means[,3],
                        sigma = cov[,,3])
nc = sum((waiting_seq[2] - waiting_seq[1]) * (duration_seq[2] - duration_seq[1]) * class_3_lhood)
class_3_norm_lhood = class_3_lhood/nc
c3_lhood_data = bind_cols(grid_data, tibble(lhood = class_3_norm_lhood))

# Class 4
class_4_lhood = dmvnorm(data = grid_data,
                        mean = means[,4],
                        sigma = cov[,,4])
nc = sum((waiting_seq[2] - waiting_seq[1]) * (duration_seq[2] - duration_seq[1]) * class_1_lhood)
class_4_norm_lhood = class_4_lhood/nc
c4_lhood_data = bind_cols(grid_data, tibble(lhood = class_4_norm_lhood))

# Plotting
mbc_viz = ggplot() +
    geom_point(data = geyser, aes(x = waiting, 
                                y = duration, 
                                color = Classes)) +
    geom_contour(data = c1_lhood_data, aes(x = Var1,
                                    y = Var2,
                                    z = lhood),
                                    color = 'red') +
    geom_contour(data = c2_lhood_data, aes(x = Var1,
                                    y = Var2,
                                    z = lhood),
                                    color = 'green') +
    geom_contour(data = c3_lhood_data, aes(x = Var1,
                                    y = Var2,
                                    z = lhood),
                                    color = 'blue') +
    geom_contour(data = c4_lhood_data, aes(x = Var1,
                                    y = Var2,
                                    z = lhood),
                                    color = 'purple') +
    labs(x = "Waiting Time",
        y = "Duration of Geyser")

ggsave(mbc_viz,
    filename = "Figures/Geyser_Clusters.png",
    scale = 2,
    height = 800,
    width = 1200,
    units = 'px')
