library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(plotly)

theme_set(theme_minimal())

plot <- combined_asian_df %>%
  filter(if (add != 8) add >= reveal else reveal %in% c(1:8)) %>%
  ggplot() +
  geom_point(mapping=aes(x=Score, y=Age.standardized.suicide.rate...Sex..both.sexes, size=Population,
                         alpha=ifelse(add == reveal, 1/5, 1/10), col=typicaled,
                         text = glue::glue('<b>Country</b>: {Country}
                                            <b>Suicide Rate</b>: {Age.standardized.suicide.rate...Sex..both.sexes}%
                                            <b>Happiness Score</b>: {Score}
                                            <b>Region</b>: {Region}
                                            <b>Population</b>: {Population}'))) +
  scale_size(range = c(1, 20)) +
  xlab("\nHappiness Score") +
  ylab("Suicide Rate") +
  labs(size = "", col = "", alpha = "") +
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous(labels = scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
  scale_y_continuous(labels = scales::number_format(suffix="%"), limits = c(0,100)) +
  theme(axis.line.x = ggplot2::element_line(colour = NULL,
                                            size = NULL, linetype = NULL, lineend = NULL),
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank())