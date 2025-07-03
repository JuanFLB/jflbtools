.plot_stat_result <- function(group, df, stats, stat_label, palette, y_label) {
  ggplot2::ggplot(df, ggplot2::aes(x = label, y = value, color = label)) +
    ggdist::stat_halfeye(
      ggplot2::aes(fill = label),
      alpha = 0.4,
      adjust = 0.5,
      width = 0.3,
      justification = -0.45,
      .width = c(0.5, 0.9),
      point_color = NA
    ) +
    ggplot2::geom_boxplot(width = 0.15, outlier.colour = NA) +
    ggplot2::geom_point(size = 1.5, alpha = 0.3,
                        position = ggplot2::position_jitter(seed = 1, width = 0.1)) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 18,
                          size = 3, color = "darkred") +
    ggplot2::scale_x_discrete(expand = c(0.1, 0.6)) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    stat_label +
    ggplot2::labs(
      title = glue::glue("{group}"),
      subtitle = paste0(
        rstatix::get_test_label(stats, detailed = FALSE, type = "text"),
        ", n = ", rstatix::get_n(stats)
      ),
      x = NULL,
      y = y_label
    ) +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::theme_bw()
}
