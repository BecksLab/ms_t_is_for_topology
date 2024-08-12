library(patchwork)
library(scales)
suppressPackageStartupMessages(library(tidyverse))

# download file from other repo
download.file("https://raw.githubusercontent.com/BecksLab/topology_generators/main/data/processed/topology_summary.csv", "../notebooks/data/topology_models.csv")
download.file("https://raw.githubusercontent.com/BecksLab/topology_generators/main/data/processed/mangal_summary.csv", "../notebooks/data/mangal_summary.csv")
download.file("https://raw.githubusercontent.com/BecksLab/topology_generators/main/data/processed/nz_summary.csv", "../notebooks/data/nz_summary.csv")


df = full_join(rbind(read.csv("../notebooks/data/mangal_summary.csv"),
                     read.csv("../notebooks/data/nz_summary.csv")),
                read.csv("../notebooks/data/topology_models.csv")) %>% 
        filter(complexity != 0) %>% 
        # to get the ratio
        mutate(ratio = top/basal,
                top = NULL,
                basal = NULL,
                ratio_mod = top_mod/basal_mod,
                top_mod = NULL,
                basal_mod = NULL) %>%
        pivot_longer(
            cols = c(complexity, distance, connectance, ratio, S1, S2, S4, S5), 
            names_to = "real",
            values_to = "real_val")  %>% 
        pivot_longer(
            cols = c(complexity_mod, distance_mod, connectance_mod, ratio_mod, S1_mod, S2_mod, S4_mod, S5_mod), 
            names_to = "test",
            values_to = "model_val")  %>% 
        mutate(
            real = str_extract(real, "[^_]*"),
            test = str_extract(test, "[^_]*"))  %>% 
        filter(real == test) %>%
        ungroup() %>% 
        filter(model_val != Inf)

# Boxplot
ggplot(df) +
    geom_boxplot(aes(x = model,
                    y = model_val,
                    colour = model),
                fill = "#ffffff00") +
    geom_hline(data = df  %>%
                group_by(real)  %>%
                reframe(mu_sim = mean(model_val, na.rm = TRUE)),
                aes(yintercept = mu_sim),
                alpha = 0.7) +
    facet_wrap(vars(real),
                scales = 'free') +
    scale_size(guide = 'none') +
    theme_classic() +
    theme(panel.border = element_rect(colour = 'black',
                                      fill = "#ffffff00"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

ggsave("../images/boxplot.png")

# z- score

ggplot(df %>% 
        group_by(id, real, model)  %>% 
        reframe(x_real = real_val,
                mu_sim = mean(model_val, na.rm = TRUE),
                sd_sim = sd(model_val, na.rm = TRUE)) %>%
        mutate(z_score = (x_real-mu_sim)) %>%
        distinct()) +
    geom_vline(aes(xintercept = 0)) +
    geom_histogram(aes(x = z_score,
                    fill = model),
                colour = "#ffffff00") +
    facet_grid(rows = vars(model),
                cols = vars(real),
                scales = "free") +
    scale_size(guide = 'none') +
    coord_cartesian(expand = FALSE) +
    theme_classic() +
    theme(panel.border = element_rect(colour = 'black',
                                      fill = "#ffffff00"))

ggsave("./images/topology.png")
