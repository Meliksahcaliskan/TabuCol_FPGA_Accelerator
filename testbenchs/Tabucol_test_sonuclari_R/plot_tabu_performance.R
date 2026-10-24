library(ggplot2)
library(dplyr)
library(rlang)

plot_tabu_performance_with_range <- function(df, 
                                             x_col, 
                                             y_col, 
                                             ymin_col, 
                                             ymax_col,
                                             fs_num,
                                             title = "DSJC250.5 Graph — 300 Rep", 
                                             subtitle = "Tabu Tenure / Fs Result Performance Curve",
                                             filename = NULL,
                                             width = 10,
                                             height = 6,
                                             dpi = 300) {
  
  p <- df |> 
    ggplot(aes(x = {{ x_col }}, y = {{ y_col }})) +
    
    # 1. Shaded Min-Max range band
    geom_ribbon(
      aes(ymin = {{ ymin_col }}, ymax = {{ ymax_col }}), 
      fill = "#00adb5", 
      alpha = 0.20
    ) +
    
    # 2. Main average performance line
    geom_line(colour = "#00adb5", linewidth = 1.2) +
    
    # 3. Boundary lines for Min and Max
    geom_line(aes(y = {{ ymin_col }}), colour = "#ff007f", linetype = "dashed", linewidth = 0.6, alpha = 0.8) +
    geom_line(aes(y = {{ ymax_col }}), colour = "#f9e2af", linetype = "dashed", linewidth = 0.6, alpha = 0.8) +
    
    # 4. Points for discrete tenure values
    geom_point(color = "#181825", fill = "#00adb5", shape = 21, size = 2.2, stroke = 0.8) +

    # 5. Labels showing fs_num at each point
    geom_text(aes(label = {{ fs_num }}), color = "#cdd6f4", size = 2.8, vjust = -1.2, fontface = "bold") +
    
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    
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
      plot.margin = margin(t = 15, r = 30, b = 15, l = 20)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = rlang::as_name(rlang::enquo(x_col)),
      y = rlang::as_name(rlang::enquo(y_col))
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
