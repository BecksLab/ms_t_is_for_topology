suppressPackageStartupMessages(library(tidyverse))
library(gt)
library(ggdendro)

# import trait table
trait_tbl <-
  read.csv("../notebooks/data/traits_table.csv")  %>% 
  tibble()

# transpose
df <- as.data.frame(t(trait_tbl[,-c(1,2)]))
colnames(df) <- pull(trait_tbl, trait)
rownames(df) <- names(trait_tbl)[-c(1,2)]

# make all NAs 0...
df[is.na(df)] <- 0

# Fitting Hierarchical clustering Model
set.seed(66)  # Setting seed
hc <- hclust(dist(df), method = "ave")
hcdata <- dendro_data(hc, type = "rectangle")

ggplot() +
  geom_segment(data = segment(hcdata),
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_text(data = label(hcdata),
    aes(x = x, y = y-0.1, label = label, hjust = 0),
    size = 3
  ) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2, 0)) +
  theme_void()

ggsave("../images/dendo.png")