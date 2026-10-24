#install.packages("tidyverse")

library(tidyverse)
library(glue)

Rep_150_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core9.csv")
Rep_200_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core8.csv")
Rep_250_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core1.csv")
Rep_300_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core2.csv")
Rep_350_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core3.csv")
Rep_400_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core4.csv")
Rep_450_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core5.csv")
Rep_500_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core6.csv")
Rep_550_single <- read_csv("../DSJC/DSJC250.5_parametre_testleri/single_test_core7.csv")

parametre_testleri_single <- bind_rows(
  Rep_150_single |> arrange(Tabu_tenure),
  Rep_200_single |> arrange(Tabu_tenure),
  Rep_250_single |> arrange(Tabu_tenure),
  Rep_300_single |> arrange(Tabu_tenure),
  Rep_350_single |> arrange(Tabu_tenure),
  Rep_400_single |> arrange(Tabu_tenure),
  Rep_450_single |> arrange(Tabu_tenure),
  Rep_500_single |> arrange(Tabu_tenure),
  Rep_550_single |> arrange(Tabu_tenure),
)

write.csv(parametre_testleri_single, "DSJC_parametre_testleri_single.csv")

for (i in 3:11) {
  parametre_testleri_single |>
    filter(Rep == i*50) |> 
    group_by(Tabu_tenure) |>  
    summarise(
      Avg_Fs = mean(Fs, na.rm = TRUE),
      min  = min(Fs, na.rm = TRUE),
      max  = max(Fs, na.rm = TRUE),
      fs_num = sum(Fs == 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    plot_tabu_performance_with_range(
      x_col    = Tabu_tenure,
      y_col    = Avg_Fs,
      ymin_col = min,
      ymax_col = max,
      fs_num   = fs_num,
      filename = glue("plots/DSJC250.5/Tabu_Tenure/Fs/DSJC250.5_Fs_performance_{i*50}_Rep.png"),
      title    = glue("DSJC250.5 Graph — Tabu Tenure / Fs Performance - w/Rep {i*50}"),
      subtitle = "Tabu Tenure / Solved Fs Result Performance Curve"
    )  
}



test_results_combined <- bind_rows(
  Rep_300_multi |> mutate(Rep = "300 Rep"),
  Rep_350_multi |> mutate(Rep = "350 Rep"),
  Rep_400_multi |> mutate(Rep = "400 Rep"),
  Rep_450_multi |> mutate(Rep = "450 Rep"),
  Rep_500_multi |> mutate(Rep = "500 Rep"),
  Rep_550_multi |> mutate(Rep = "550 Rep")
)

print(test_results_combined, n = 25)
glimpse(Rep_550_multi)



source("plot_tabu_performance.R")



library(ggplot2)
library(dplyr)
library(rlang)

source("plot_tabu_performance_by_group.R")

plot_tabu_performance_by_group(
  df = test_results_combined,
  x_col = Tabu_tenure,
  y_col = Fs,
  group_col = Rep,
  filename = "plots/DSJC250.5/DSJC250.5_performance_comparison.png"
)
