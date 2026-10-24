plot_tabu_performance_by_group <- function(df,
                                           x_col,
                                           y_col,
                                           group_col,
                                           title = "DSJC250.5 Graph — Tabu Tenure vs Fs",
                                           subtitle = "Performance comparison across replications",
                                           filename = NULL,
                                           width = 10,
                                           height = 6,
                                           dpi = 300,
                                           x_angle = 0,
                                           y_trans = "identity"
) {

  p <- df |>
    mutate(..group = as.factor({{ group_col }})) |>
    ggplot(aes(x = {{ x_col }}, y = {{ y_col }}, color = ..group, fill = ..group, group = ..group)) +
    geom_area(alpha = 0.08, position = "identity") +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2, alpha = 0.8) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(trans = y_trans, labels = scales::number_format(accuracy = 0.01)) +
    
    # High-contrast color palette for dark background
    scale_color_manual(values = c("#00adb5", "#ff007f", "#00ff88", "#f9e2af", "#cba6f7", "#89b4fa")) +
    scale_fill_manual(values = c("#00adb5", "#ff007f", "#00ff88", "#f9e2af", "#cba6f7", "#89b4fa")) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "#1e1e2e", color = NA),
      panel.background = element_rect(fill = "#181825", color = NA),
      panel.grid.major = element_line(color = "#313244", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#cdd6f4"),
      axis.text = element_text(color = "#bac2de"),
      axis.title = element_text(color = "#cdd6f4", face = "bold"),
      plot.title = element_text(color = "#00ff88", face = "bold", size = 13),
      plot.subtitle = element_text(color = "#a6adc8", size = 10),
      plot.margin = margin(t = 15, r = 30, b = 15, l = 20),
      
      axis.text.x = element_text(angle = x_angle, hjust = if (x_angle > 0) 1 else 0.5),

      # Legend customization
      legend.position = "top",
      legend.background = element_rect(fill = "#181825", color = NA),
      legend.key = element_rect(fill = "#181825", color = NA),
      legend.text = element_text(color = "#cdd6f4"),
      legend.title = element_text(color = "#00ff88", face = "bold")
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = rlang::as_name(rlang::enquo(x_col)),
      y = rlang::as_name(rlang::enquo(y_col)),
      color = rlang::as_name(rlang::enquo(group_col)),
      fill = rlang::as_name(rlang::enquo(group_col))
    )
  
  # Save plot if a filename path is provided
  if (!is.null(filename)) {
    ggsave(
      filename = filename,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "#1e1e2e" # Ensures dark background is retained in saved file
    )
  }
  
  # Return the ggplot object so it renders in RStudio/Console
  return(p)
  
}
