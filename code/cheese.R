cheese_comm <- tribble(
  ~shop,                ~roquefort, ~mozzerella, ~swiss, ~muenster, ~gouda, ~brie,
  "Let it Brie",           10,            8,        0,       0,        4,     5,
  "Grater Gouda",           9,            8,        6,       0,        6,     5,
  "Praise Cheeses",         8,           10,        5,       6,        0,     0,
  "C'est Cheese",          11,            8,        6,       0,        0,     4,
  "Dis a Brie",            10,           11,        4,       0,        0,     6,
  "Cheese Louise",          0,            4,        5,       4,        6,     8,
  "Parmartisan",            2,            3,        4,       5,        0,     4,
  "Fromagination",          8,            4,        5,       3,        9,     0,
  "Grapes and Curd",        4,            4,        5,       5,        0,     2,
  "The Curdles",            0,            5,        1,       0,        6,    10,
  "Fetaphysics",            2,            4,        1,       0,        3,     2,
  "Paris Stilton",          0,            0,        2,       0,        2,     3,
  "A Curd in the Hand",     4,            0,        1,       0,        4,     4,
  "Grater Plans",           0,            0,        1,       0,        2,     3,
  "Curd of Prey",           0,            3,        1,       0,        3,     2,
) %>% 
  column_to_rownames("shop")

cheese_env <- tribble(
  ~shop,                    ~wine,      ~clever_pun, ~pretentious, ~fun,
  "Let it Brie",       "Bordeaux",        5,           10,          1,
  "Grater Gouda",      "Bordeaux",        4,           9,           3,
  "Praise Cheeses",    "Bordeaux",        5,           8,           2, 
  "C'est Cheese",      "Bordeaux",        3,           7,           3, 
  "Dis a Brie",        "Bordeaux",        3,           8,           2,
  "Cheese Louise",   "Chardonnay",        5,           6,           4, 
  "Parmartisan",     "Chardonnay",        5,           4,           5, 
  "Fromagination",   "Chardonnay",        5,           5,           6,
  "Grapes and Curd", "Chardonnay",        3,           4,           5,
  "The Curdles",     "Chardonnay",        3,           5,           5,
  "Fetaphysics",       "Riesling",        5,           1,           6,
  "Paris Stilton",     "Riesling",        2,           2,           7,
  "A Curd in the Hand","Riesling",        1,           3,           8,
  "Grater Plans",      "Riesling",        2,           2,           9,
  "Curd of Prey",      "Riesling",        5,           1,           10,
)

cheese_meta <- cheese_env %>% 
  select(shop, wine)

cheese_rich <- specnumber(cheese_comm)
cheese_rich

cheese_rich_df <- cheese_rich %>% 
  enframe() %>% 
  full_join(., cheese_meta, by = c("name" = "shop")) %>% 
  group_by(wine) %>% 
  summarize(mean_rich = mean(value),
            err_rich = sd(value)/sqrt(length(value)))

pal <- c("lightsalmon1", "gold1", "palegreen4")

cheese_col <- ggplot(cheese_rich_df, aes(x = wine, y = mean_rich, fill = wine)) +
  geom_col(color = "gray25") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(x = wine, ymin = mean_rich - err_rich, ymax = mean_rich + err_rich), width = 0.5, color = "gray25") +
  scale_x_discrete(labels = c("Bordeaux \n (n = 5)", "Chardonnay \n (n = 5)", "Riesling \n (n = 5)")) +
  scale_y_continuous(limits = c(0, 5.25), expand = c(0,0)) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12)) + 
  labs(x = "Wine",
       y = "Number of cheeses",
       title = "Cheese species richness")
cheese_col

ggsave("cheese_rich.jpg", cheese_col, width = 5, height = 3, dpi = 200)
            
cheese_div <- cheese_comm %>% 
  # calculate Shannon diversity for each sample - Shannon is the default
  diversity() %>% 
  # put all those calculations into a tibble
  enframe() %>% 
  # rename columns for ease of joining
  rename(shop = name,
         shan_div = value) %>% 
  # join with cheese_meta
  full_join(cheese_meta, ., by = "shop") %>% 
  # group by wine
  group_by(wine) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = mean(shan_div),
            err = sd(shan_div)/sqrt(length(shan_div)))
cheese_div

cheese_div_col <- ggplot(cheese_div, aes(x = wine, y = mean, fill = wine)) +
  geom_col(color = "gray25") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(x = wine, ymin = mean - err, ymax = mean + err), width = 0.5, color = "gray25") +
  scale_x_discrete(labels = c("Bordeaux \n (n = 5)", "Chardonnay \n (n = 5)", "Riesling \n (n = 5)")) +
  scale_y_continuous(limits = c(0, 1.65), expand = c(0,0)) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12)) + 
  labs(x = "Wine",
       y = "Number of cheeses",
       title = "Cheese Shannon diversity")
cheese_div_col

ggsave("cheese_div.jpg", cheese_div_col, width = 5, height = 3, dpi = 200)


cheese_perm <- adonis(cheese_comm ~ wine, data = cheese_env)
cheese_perm

cheese_NMDS <- metaMDS(cheese_comm)
cheese_NMDS
plot(cheese_NMDS)

cheese_NMDS_df <- as_tibble(cheese_NMDS$points) %>% 
  bind_cols(cheese_meta)


cheese_NMDS_plot <- ggplot(cheese_NMDS_df, aes(x = MDS1, y = MDS2, color = wine)) +
  geom_text(aes(label = shop), size = 5) + 
  scale_color_manual(values = pal) +
  stat_ellipse(size = 0.75) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12))
  
cheese_NMDS_plot

ggsave("cheese_nmds.jpg", cheese_NMDS_plot, width = 7, height = 4, dpi = 200)

cheese_envfit <- envfit(cheese_NMDS, env = cheese_env)
cheese_envfit

cheese_vectors_df <- data.frame(cheese_envfit$vectors$arrows)

cheese_with_vectors <- ggplot(cheese_NMDS_df) +
  geom_point(aes(x = MDS1, y = MDS2, color = wine, shape = wine), size = 3, alpha = 0.8) + 
  stat_ellipse(aes(x = MDS1, y = MDS2, color = wine), linetype = 2, size = 1) +
  geom_text(data = cheese_vectors_df, aes(x = NMDS1, y = NMDS2, label = rownames(cheese_vectors_df))) +
  geom_segment(data = cheese_vectors_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2))
cheese_with_vectors

cheese_CCA <- cca(cheese_comm ~ pretentious*fun, data = cheese_env)
cheese_CCA

autoplot(cheese_CCA)

cheese_CCA_df <- fortify(cheese_CCA)

cheese_shops <- cheese_CCA_df %>% 
  filter(Score == "sites")

cheese_types <- cheese_CCA_df %>% 
  filter(Score == "species")

cheese_vectors <- cheese_CCA_df %>% 
  filter(Score == "biplot")

cheese_CCA_gg <- ggplot(cheese_shops, aes(x = CCA1, y = CCA2)) +
  geom_text(aes(label = Label), color = "red") +
  geom_text(data = cheese_types, aes(label = Label), color = "blue") +
  geom_segment(data = cheese_vectors, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(angle = 30, length = unit(0.1, "inches"))) +
  geom_text(data = cheese_vectors, aes(label = Label))
  
cheese_CCA_gg








