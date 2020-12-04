##########    utils_mspa.R
##
### Author: tkeyes
##
### Description: 
##    Loads some custom functions and other baubles for use in MSPA data analysis. 
##
######################################################################################

# Parameters

likert_colors <- c("darkgreen","green","orange","red","darkred")
mspa_hex <- "#666699"

# Libraries

library(waffle)

####==================================================================================


### Description: 
#     Plots a stacked bar plot between LGBTQ+ and non-LGBTQ+ individuals when the likert scale is 
#     encoded by the variable my_var. Will provide the plot with the title `title` and the caption 
#     N = `n`. 
#
### Inputs: 
#       - my_data = tibble containing the data to be plotted
#       - my_var = unquoted variable name to make a facet plot out of. 
#       - break_var = variable to break up data in my_var
#       - title = title to put on top of the plot
#       - n = number of observations to add to the caption of the plot
###############################################################################
likert_plot <- 
  function(my_data = NULL, my_var = NULL, break_var = NULL, title = NULL, n = NULL) { 
    my_data %>% 
      drop_na({{my_var}}, {{break_var}}) %>% 
      count({{my_var}}, {{break_var}}) %>% 
      group_by({{break_var}}) %>% 
      mutate(proportion = n / sum(n)) %>% 
      ggplot(
        aes(
          x = {{break_var}}, 
          y = proportion, 
          fill = factor({{my_var}})#, levels = as.character(5:1))
        )
      ) + 
      geom_col() + 
      geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
      scale_fill_brewer(
        #labels = rev(likert_labels), # will have to change
        type = "div",
        palette = "RdYlGn", 
        aesthetics = "fill", 
        direction = 1, 
        guide = guide_legend(reverse = TRUE)
      ) + 
      coord_flip() + 
      theme_minimal() + 
      theme(
        legend.title = element_text(size = 0.5),
        aspect.ratio = 0.4, 
        legend.position = "bottom"
      ) +  
      labs(
        subtitle = title,
        x = NULL,
        y = "Percentage of respondents", 
        fill = NULL,
        caption = str_glue("N = {n}")
      )
  }
