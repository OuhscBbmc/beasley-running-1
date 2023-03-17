library(ggplot2)
ds <-
  tibble::tibble(
  runner = c("Willie boy v",
                        "Willie boy v","Willie boy v","Willie boy v","Willie boy v",
                        "Old Man iv","Old Man iv","Old Man iv","Old Man iv",
                        "Old Man iv","Mom","Mom","Mom","Mom","Mom","Bennett",
                        "Bennett","Bennett","Bennett","Bennett","Mr. Guy",
                        "Mr. Guy","Mr. Guy","Mr. Guy","Mr. Guy","Preston",
                        "Preston","Preston","Preston","Preston","Liam","Liam",
                        "Liam","Liam","Liam"),
               food = c("nothing","gummies",
                        "chocholate","corn","granola","nothing","gummies",
                        "chocholate","corn","granola","nothing","gummies",
                        "chocholate","corn","granola","nothing",
                        "gummies","chocholate","corn","granola","nothing",
                        "gummies","chocholate","corn","granola","nothing",
                        "gummies","chocholate","corn","granola",
                        "nothing","gummies","chocholate","corn","granola"),
               time = c("6:24","6:12","6:09",
                        "6:29","6:20","7:04","6:54","6:44","7:00","6:48",
                        "9:56","9:39","9:34","9:47","9:38","8:11","7:51",
                        "7:31","8:10","7:34","9:59","9:51","9:39","9:56",
                        "9:49","7:20","7:08","6:59","7:13","7:11","8:12",
                        "8:03","7:49","7:58","7:51")
)

ds <-
  ds |>
  dplyr::mutate(
    minutes = as.integer(sub("^(\\d{1,2}):(\\d{2})$", "\\1", time)),
    seconds = as.integer(sub("^(\\d{1,2}):(\\d{2})$", "\\2", time)),
    duration = 60 * minutes + seconds,
  ) |>
  dplyr::mutate(
    food = factor(food, c("nothing", "gummies", "chocholate", "corn", "granola")),
    runner = factor(runner, c("Mr. Guy", "Mom", "Liam", "Bennett", "Preston", "Old Man iv", "Willie boy v")),
  ) |>
  dplyr::select(
    -time,
    -minutes,
    -seconds,
  ) |>
  dplyr::group_by(runner) |>
  dplyr::mutate(
    diff = duration - mean(duration)
  ) |>
  dplyr::ungroup()

ggplot(ds, aes(x = food, y = duration, group = runner, color = runner)) +
  geom_hline(yintercept = 6:10 * 60, linetype = "82", color = "#2222ff44") +
  annotate("text", x=Inf, y=6:10 * 60, label = paste(6:10, "min"), hjust = 1, color = "#2222ff44") +
  geom_point(size = 1, alpha = .7, key_glyph = draw_key_rect) +
  geom_line(linewidth = 4, alpha = .7, key_glyph = draw_key_rect) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title     = "Mile Time by Carbohydrate",
    subtitle  = "for each runner",
    x         = "Carbohydrate Complexity\n(from least to most)",
    y         = "Duration (sec)",
    color     = "Runner"
  )

ggsave(
  "time-by-food.png",
  dpi = 1200,
  height = 5.5,
  width = 8,
  scale = 1,
  units = "in",
  bg    = "#ffffff"
)

ggplot(ds, aes(x = food, y = diff, group = runner, color = runner)) +
  geom_hline(yintercept = 0, linetype = "82", color = "#91a3f5") +
  annotate("text", x=-Inf, y=0, label = "runner's\nmean", hjust = 0, color = "#2222ff44") +
  geom_point(size = 1, alpha = .7, key_glyph = draw_key_rect) +
  geom_line(linewidth = 1, alpha = .7, key_glyph = draw_key_rect) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title     = "Diff Score = [duration] - [runner's mean duration]",
    subtitle  = "",
    x         = "Carbohydrate Complexity\n(from least to most)",
    y         = "Difference from Runner's mean (sec)",
    color     = "Runner"
  )

ggsave(
  "diff-1.png",
  dpi = 1200,
  height = 5.5,
  width = 8,
  scale = 1,
  units = "in",
  bg    = "#ffffff"
)

ggplot(ds, aes(x = food, y = diff, group = runner, color = runner)) +
  geom_hline(yintercept = 0, linetype = "82", color = "#91a3f5") +
  annotate("text", x=-Inf, y=0, label = "runner's\nmean", hjust = 0, color = "#2222ff44") +
  geom_smooth(aes(group = 1, color = NULL), linewidth = 3, color = "#1765a2") +
  geom_point(size = 1, alpha = .3, key_glyph = draw_key_rect) +
  geom_line(linewidth = 1, alpha = .3, key_glyph = draw_key_rect) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title     = "Diff Score = [duration] - [runner's mean duration]",
    subtitle  = "(The blue line represents the mean score for each food)",
    x         = "Carbohydrate Complexity\n(from least to most)",
    y         = "Difference from Runner's mean (sec)",
    color     = "Runner"
  )

ggsave(
  "diff-2.png",
  dpi = 1200,
  height = 5.5,
  width = 8,
  scale = 1,
  units = "in",
  bg    = "#ffffff"
)
