# plots.R

# ---- Optional: Define theme (not used) ----
# material <- bs_theme(
#   bg = "white", 
#   fg = "black", 
#   primary = "red", 
#   secondary = "blue",
#   success = "#4F9B29",
#   info = "#28B3ED",
#   warning = "#FD7424",
#   danger = "#F7367E",
#   base_font = font_google("Open Sans"),
#   heading_font = font_google("Proza Libre"),
#   code_font = font_google("Fira Code")
# )



make_dist_plot <- function(mean_diff, sd) {
  thematic_on()
  
  x <- 1 + rnorm(1000, 80, 1 + sd)
  y <- 1 + rnorm(1000, 80 + mean_diff, 1 + sd)
  z <- rep(c("A", "B"), each = 500)
  
  df <- data.frame(x, y, z) %>%
    pivot_longer(!z, names_to = "Group", values_to = "Outcome")
  
  ggplot(df, aes(x = Outcome, color = Group, fill = Group)) +
    geom_density(alpha = 0.5) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")
}

make_cor_plot <- function(corr) {
  set.seed(6687)
  
  x <- 1 + rnorm(1000, 100, 500)
  random1 <- runif(1000, min = min(x), max = max(x))
  y <- x * corr + random1 * (1 - corr)
  
  df <- data.frame(x = x, y = y)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_jitter() +
    geom_smooth(method = lm, se = FALSE) +
    theme_bw(base_size = 20) +
    annotate("text", x = 0, y = -1700, label = paste("R =", round(cor(df$x, df$y), 2)), size = 5)
}

make_power_plot <- function(effect1, effect2) {
  power_data <- map_dfr(4:500, ~{
    t1 <- pwr.r.test(r = effect1, sig.level = 0.05, n = .x)
    t2 <- pwr.r.test(r = effect2, sig.level = 0.05, n = .x)
    tibble(n = .x, `r = 0,1` = t1$power, `r = 0,2` = t2$power)
  })
  
  temp <- power_data %>%
    pivot_longer(-n, names_to = "group", values_to = "power")
  
  ggplot(temp, aes(x = n, y = power, color = group, linetype = group)) +
    geom_hline(yintercept = 0.80, linetype = 1, size = 1.2, color = "darkgray") +
    geom_line(linewidth = 1) +
    theme_bw(base_size = 20) +
    geom_label(data = temp %>% filter(n == 50),
               aes(label = paste0("d: ", c(effect1, effect2)), fill = group),
               size = 5, color = "white") +
    ylab("Power") +
    xlab("N") +
    theme(legend.position = "none")
}

make_low_power_plot <- function(max_cases) {
  power_data <- map_dfr(4:max_cases, ~{
    t1 <- pwr.r.test(r = 0.1, sig.level = 0.05, n = .x)
    t2 <- pwr.r.test(r = 0.2, sig.level = 0.05, n = .x)
    t3 <- pwr.r.test(r = 0.3, sig.level = 0.05, n = .x)
    tibble(n = .x,
           `d = 0,1` = t1$power,
           `d = 0,2` = t2$power,
           `d = 0,3` = t3$power)
  })
  
  temp <- power_data %>%
    pivot_longer(-n, names_to = "group", values_to = "power")
  
  ggplot(temp, aes(x = n, y = power, color = group, linetype = group)) +
    geom_hline(yintercept = 0.80, linetype = 1, color = "darkgray", size = 1.2) +
    geom_line(linewidth = 1) +
    theme_bw(base_size = 20) +
    geom_label(data = temp %>% filter(n == 50),
               aes(label = group, fill = group),
               size = 5, color = "white") +
    ylab("Power") +
    xlab("N") +
    theme(legend.position = "none")
}

make_n_plot <- function(d, power) {
  df <- pwr.t.test(d = d, sig.level = 0.05, power = power, type = "two.sample")
  n <- round(df$n)
  
  data.frame(x = paste0("N\nn=", n), y = runif(n)) %>%
    ggplot(aes(x = x, y = y, color = y)) +
    geom_quasirandom(size = 3) +
    theme_bw(base_size = 24) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank()
    )
}
