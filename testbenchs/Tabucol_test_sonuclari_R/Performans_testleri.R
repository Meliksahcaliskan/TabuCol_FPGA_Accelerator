library(tidyverse)

DSJC125.5_C_single <- read.csv("../DSJC/performans_testleri/DSJC125.5/DSJC125.5_C_single.csv")


library(glue)

testbench <- data.frame(
  klasor = c("DSJC", "DSJC", "DSJC", "DSJC", "flat", "le", "le", "le", "le"),
  ad     = c("DSJC125.1", "DSJC125.5", "DSJC125.9", "DSJC250.1",
             "flat300_20_0", "le450_15a", "le450_15b", "le450_25a", "le450_25b")
)

oku <- function(klasor, ad) {
  yol <- glue("../{klasor}/performans_testleri/{ad}/{ad}")
  bind_rows(
    read.csv(glue("{yol}_C_single.csv")) |>  mutate(mode = "C", data = ad),
    read.csv(glue("{yol}_bitbase_single.csv")) |>  mutate(mode = "bitbase", data = ad)
  )
}

single_performans_tests <- setNames(
  Map(oku, testbench$klasor, testbench$ad),
  testbench$ad
)



names(single_performans_tests)

ready_single_performans <- bind_rows(single_performans_tests)

write.csv(ready_single_performans, "Tables/Single_performans_test.csv")

ready_single_performans <- read.csv("Tables/Single_performans_test.csv")

view(ready_single_performans)

view(ready_single_performans[["DSJC125.5"]])


source("plot_tabu_performance.R")
source("plot_tabu_performance_by_group.R")

testbench_summary <- ready_single_performans |>
  group_by(data, mode) |>
  summarise(Avg_Solve_time = mean(Solve_time, na.rm = TRUE), .groups = "drop")

testbench_ratio_labels <- testbench_summary |>
  pivot_wider(names_from = mode, values_from = Avg_Solve_time) |>
  mutate(
    ratio       = round(C / bitbase, 2),
    ratio_label = ifelse(is.finite(ratio), paste0("×", ratio), "—"),
    label_y     = pmax(C, bitbase, na.rm = TRUE)
  ) |>
  select(data, ratio_label, label_y)

testbench_summary |>
  plot_tabu_performance_by_group(
    x_col     = data,
    y_col     = Avg_Solve_time,
    group_col = mode,
    x_angle   = 45,
    y_trans   = "sqrt",
    title     = "Testbench Performans Karşılaştırması — C vs Bitbase",
    subtitle  = "Testbench / Ortalama Çözüm Süresi | Etiket: C/Bitbase Hız Oranı",
    filename  = "Performans_Testleri/Testbench_Avg_Solve_Time_C_vs_Bitbase.png"
  ) +
  geom_label(
    data          = testbench_ratio_labels,
    aes(x = data, y = label_y, label = ratio_label),
    inherit.aes   = FALSE,
    vjust         = -0.4,
    color         = "#f9e2af",
    fill          = "#313244",
    size          = 3,
    fontface      = "bold",
    label.size    = 0.3,
    label.padding = unit(0.15, "lines")
  )

performans_testi <- performans_testi |> 
  group_by(Mode) |> 
  group_by(Dataset) |> 
  mutate(
    Avg_solve_time = mean(Solve_time, na.rm = TRUE),
    min_solve_time = min(Solve_time, na.rm = TRUE),
    max_solve_time = max(Solve_time, na.rm = TRUE)
  )

performans_testi |> 
  filter(Solve_count == 0) |> 
  plot_tabu_performance_by_group(
    x_col = Dataset,
    y_col = Avg_solve_time,
    group_col = Mode,
    title = "C ve Bitbase Performance Karşılaştırması",
    subtitle = "Kolay graphlar",
    filename = "Performans_Testleri/Bitbase_Performance_Test_0.png"
  )
  


  