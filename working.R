weight_room_assessments_velo_investigation2 <- reactive({
  
# "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
# norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric

if( input$norms_input_qs == 2)
{dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,age_group, assessment, metric), date==min(date))) %>%
    dplyr::filter(age_group %in% input$norms_input_groups_qs)
}
else if( input$norms_input_qs == 3)
{dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,position, assessment, metric), date==min(date))) %>%
    dplyr::filter(position %in% input$norms_input_groups_qs)
}
else if( input$norms_input_qs == 4)
{dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,laterality, assessment, metric), date==min(date))) %>%
    dplyr::filter(laterality %in% input$norms_input_groups_qs)
}
else
{}

})


################################################################################
# Rainclouds by Input
################################################################################
# "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
# norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric

ridge_plots_qs <- reactive({
  
   if (input$norms_input_qs == 2) # "Age Group" = 2

  {  individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2() %>% 
    dplyr::filter(!is.na(weight_room_assessments_velo_investigation2()[, input$x])) %>% 
    dplyr::filter(!is.na(age_group))
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    mutate(x = individual_athlete_data_final2_ridge[, input$x])
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    dplyr::group_by(age_group) %>% 
    mutate(
      n = n(),
      median = median(x),
      max = max(x),
      Velo_Group2 = age_group
    ) %>% 
    ungroup()
  
  individual_athlete_data_final2_ridge <- 
    individual_athlete_data_final2_ridge %>% 
    #filter(n >= 10) %>% 
    mutate(species_num = as.numeric(fct_rev(age_group))) %>% 
    ungroup() 
  
  ## create a second chart with raincloud plotsNew_RSI
  ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge[, input$x]), individual_athlete_data_final2_ridge$species_num, color = age_group)) +
    stat_summary(
      geom = "linerange",
      fun.min = function(x) -Inf,
      fun.max = function(x) median(x, na.rm = TRUE),
      linetype = "dotted",
      orientation = "y",
      size = .7
    ) +
    geom_point(
      aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
      shape = "|",
      size = 5,
      alpha = .5
    ) +
    ggdist::stat_halfeye(
      aes(
        y = individual_athlete_data_final2_ridge$species_num,
        color = age_group,
        fill = after_scale(colorspace::lighten(color, .2))
      ),
      shape = 18,
      point_size = 3,
      interval_size = 1.8,
      adjust = .5,
      .width = c(0, 1)
    ) +
    geom_text(
      aes(x = median, label = format(round(median, 2), nsmall = 2)),
      stat = "unique",
      color = "white",
      family = "gotham",
      fontface = "bold",
      size = 3.4,
      nudge_y = .15
    ) +
    geom_text(
      aes(x = max, label = glue::glue("n = {n}")),
      stat = "unique",
      family = "gotham",
      fontface = "bold",
      size = 3.5,
      hjust = 0,
      nudge_x = .01,
      nudge_y = .02
    ) +
    scale_y_continuous(
      limits = c(.55, NA),
      breaks = 1:1,
      labels = paste0(individual_athlete_data_final2_ridge$age_group)
    ) +
    scale_color_manual(values = c("#000FFF"), guide = "none") +
    scale_fill_manual(values = c("#000FFF"), guide = "none") +
    labs(
      x = paste0(input$x),
      y = NULL,
      title = paste0(input$x, " by Age Group"),
      subtitle = paste0("Where an athlete stacks up vs peers"),
      caption = "Plot by @aosnacz"
    ) +
    theme(panel.background = element_blank()) +
    geom_vline(xintercept = individual_athlete_data_final2()$value, colour="black", linetype = "dashed", size = 1, alpha = .5) +
    facet_wrap(individual_athlete_data_final2()$assessment~individual_athlete_data_final2()$metric)
   }
  else if(input$norms_input_qs == 3) # "Position" = 3

  {individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2() %>% 
    dplyr::filter(!is.na(weight_room_assessments_velo_investigation2()[, input$x])) %>% 
    dplyr::filter(!is.na(position))
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    mutate(x = individual_athlete_data_final2_ridge[, input$x])
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    dplyr::group_by(position) %>% 
    mutate(
      n = n(),
      median = median(x),
      max = max(x),
      Velo_Group2 = position
    ) %>% 
    ungroup()
  
  individual_athlete_data_final2_ridge <- 
    individual_athlete_data_final2_ridge %>% 
    #filter(n >= 10) %>% 
    mutate(species_num = as.numeric(fct_rev(position))) %>% 
    ungroup() 
  
  ## create a second chart with raincloud plotsNew_RSI
  ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge[, input$x]), individual_athlete_data_final2_ridge$species_num, color = position)) +
    stat_summary(
      geom = "linerange",
      fun.min = function(x) -Inf,
      fun.max = function(x) median(x, na.rm = TRUE),
      linetype = "dotted",
      orientation = "y",
      size = .7
    ) +
    geom_point(
      aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
      shape = "|",
      size = 5,
      alpha = .5
    ) +
    ggdist::stat_halfeye(
      aes(
        y = individual_athlete_data_final2_ridge$species_num,
        color = position,
        fill = after_scale(colorspace::lighten(color, .2))
      ),
      shape = 18,
      point_size = 3,
      interval_size = 1.8,
      adjust = .5,
      .width = c(0, 1)
    ) +
    geom_text(
      aes(x = median, label = format(round(median, 2), nsmall = 2)),
      stat = "unique",
      color = "white",
      family = "gotham",
      fontface = "bold",
      size = 3.4,
      nudge_y = .15
    ) +
    geom_text(
      aes(x = max, label = glue::glue("n = {n}")),
      stat = "unique",
      family = "gotham",
      fontface = "bold",
      size = 3.5,
      hjust = 0,
      nudge_x = .01,
      nudge_y = .02
    ) +
    scale_y_continuous(
      limits = c(.55, NA),
      breaks = 1:1,
      labels = paste0(individual_athlete_data_final2_ridge$position)
    ) +
    scale_color_manual(values = c("#000FFF"), guide = "none") +
    scale_fill_manual(values = c("#000FFF"), guide = "none") +
    labs(
      x = paste0(input$x),
      y = NULL,
      title = paste0(input$x, " by Position"),
      subtitle = paste0("Where an athlete stacks up vs peers"),
      caption = "Plot by @aosnacz"
    ) +
    theme(panel.background = element_blank()) +
    geom_vline(xintercept = individual_athlete_data_final2()$value, colour="black", linetype = "dashed", size = 1, alpha = .5) +
    facet_wrap(individual_athlete_data_final2()$assessment~individual_athlete_data_final2()$metric)
  }
  else if(input$norms_input_qs == 4) # "Laterality" = 4
  { individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2() %>% 
    dplyr::filter(!is.na(weight_room_assessments_velo_investigation2()[, input$x])) %>% 
    dplyr::filter(!is.na(laterality))
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    mutate(x = individual_athlete_data_final2_ridge[, input$x])
  
  individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
    dplyr::group_by(laterality) %>% 
    mutate(
      n = n(),
      median = median(x),
      max = max(x),
      Velo_Group2 = laterality
    ) %>% 
    ungroup()
  
  individual_athlete_data_final2_ridge <- 
    individual_athlete_data_final2_ridge %>% 
    #filter(n >= 10) %>% 
    mutate(species_num = as.numeric(fct_rev(laterality))) %>% 
    ungroup() 
  
  ## create a second chart with raincloud plotsNew_RSI
  ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge[, input$x]), individual_athlete_data_final2_ridge$species_num, color = laterality)) +
    stat_summary(
      geom = "linerange",
      fun.min = function(x) -Inf,
      fun.max = function(x) median(x, na.rm = TRUE),
      linetype = "dotted",
      orientation = "y",
      size = .7
    ) +
    geom_point(
      aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
      shape = "|",
      size = 5,
      alpha = .5
    ) +
    ggdist::stat_halfeye(
      aes(
        y = individual_athlete_data_final2_ridge$species_num,
        color = laterality,
        fill = after_scale(colorspace::lighten(color, .2))
      ),
      shape = 18,
      point_size = 3,
      interval_size = 1.8,
      adjust = .5,
      .width = c(0, 1)
    ) +
    geom_text(
      aes(x = median, label = format(round(median, 2), nsmall = 2)),
      stat = "unique",
      color = "white",
      family = "gotham",
      fontface = "bold",
      size = 3.4,
      nudge_y = .15
    ) +
    geom_text(
      aes(x = max, label = glue::glue("n = {n}")),
      stat = "unique",
      family = "gotham",
      fontface = "bold",
      size = 3.5,
      hjust = 0,
      nudge_x = .01,
      nudge_y = .02
    ) +
    scale_y_continuous(
      limits = c(.55, NA),
      breaks = 1:1,
      labels = paste0(individual_athlete_data_final2_ridge$laterality)
    ) +
    scale_color_manual(values = c("#000FFF"), guide = "none") +
    scale_fill_manual(values = c("#000FFF"), guide = "none") +
    labs(
      x = paste0(input$x),
      y = NULL,
      title = paste0(input$x, " by Laterality"),
      subtitle = paste0("Where an athlete stacks up vs peers"),
      caption = "Plot by @aosnacz") +
    theme(panel.background = element_blank()) +
    geom_vline(xintercept = individual_athlete_data_final2()$value, colour="black", linetype = "dashed", size = 1, alpha = .5) +
    facet_wrap(individual_athlete_data_final2()$assessment~individual_athlete_data_final2()$metric)
  }
  else {}
})

output$ridge_plots_qs_output <- renderPlot({ridge_plots_qs()})

