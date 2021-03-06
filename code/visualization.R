# set-up =======================================================================

library(ggplot2)
library(here)

setosa_subset <- read.csv(here("./output/setosa_subset_data.csv"))

# make and save a simple plot
simple_fig = ggplot(data = setosa_subset) +
  geom_point(aes(x = Sepal.Length, y = Petal.Width)) +
  labs(x = "sepal length", y = "petal width") +
  theme_bw()
ggsave(here('./figs/simple_iris_fig.png'), simple_fig)

new_data <- read.csv(here("./output/new_data_for_prediction.csv"))

# plot prediction
prediction_plot = ggplot(data = new_data) +
  geom_line(aes(x = Petal.Length, y = prediction),
            size = 2, colour = 'red') +
  labs(x = "petal length", y = "sepal length prediction") +
  theme_bw()
ggsave(here('./figs/prediction_plot_simple.png'), prediction_plot)
