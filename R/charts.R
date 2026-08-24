theme_kazanma <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#101815", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#101815", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#27332E", linewidth = .35),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = "#EAF0EC"),
      axis.text = ggplot2::element_text(colour = "#B8C4BD"),
      axis.title = ggplot2::element_text(colour = "#8F9C95"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(colour = "#8F9C95"),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "#C9D3CD")
    )
}

plot_outcomes <- function(prediction) {
  df <- tibble::tibble(
    result = factor(c(prediction$home$team, "Beraberlik", prediction$away$team), levels = c(prediction$home$team, "Beraberlik", prediction$away$team)),
    probability = as.numeric(prediction$outcomes),
    color = c("home", "draw", "away")
  )
  ggplot2::ggplot(df, ggplot2::aes(result, probability, fill = color)) +
    ggplot2::geom_col(width = .64, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(probability, accuracy = 1)), vjust = -0.55, colour = "#F5F8F6", fontface = "bold", size = 4.2) +
    ggplot2::scale_fill_manual(values = c(home = "#D7A84B", draw = "#68756E", away = "#63B4A5")) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, max(df$probability) * 1.22), expand = c(0, 0)) +
    ggplot2::labs(title = "Maç sonucu olasılığı", subtitle = "Süper Lig takım gücü + taktik eşleşmeli Poisson ön-modeli", x = NULL, y = "Olasılık") +
    theme_kazanma()
}

plot_score_matrix <- function(prediction) {
  df <- as.data.frame(as.table(prediction$score_matrix / sum(prediction$score_matrix)))
  names(df) <- c("home_goals", "away_goals", "probability")
  df <- df |> dplyr::mutate(home_goals = as.integer(as.character(home_goals)), away_goals = as.integer(as.character(away_goals)))

  ggplot2::ggplot(df, ggplot2::aes(away_goals, home_goals, fill = probability)) +
    ggplot2::geom_tile(colour = "#17221D", linewidth = .45) +
    ggplot2::geom_text(
      data = df |> dplyr::filter(probability >= .035),
      ggplot2::aes(label = scales::percent(probability, accuracy = 1)),
      colour = "#F7F3E8", size = 3.2
    ) +
    ggplot2::scale_fill_gradient(low = "#1E2D27", high = "#D7A84B", labels = scales::percent) +
    ggplot2::scale_x_continuous(breaks = 0:6) +
    ggplot2::scale_y_continuous(breaks = 0:6) +
    ggplot2::labs(title = "Skor matrisi", subtitle = "Hücreler kesin skor olasılığını gösterir", x = paste(prediction$away$short, "gol"), y = paste(prediction$home$short, "gol"), fill = "Olasılık") +
    theme_kazanma() +
    ggplot2::theme(legend.position = "none")
}

plot_styles <- function(prediction) {
  ggplot2::ggplot(prediction$styles, ggplot2::aes(value, metric, colour = team, group = metric)) +
    ggplot2::geom_line(colour = "#34423B", linewidth = 1.2) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_colour_manual(values = setNames(c("#D7A84B", "#63B4A5"), c(prediction$home$team, prediction$away$team))) +
    ggplot2::scale_x_continuous(limits = c(35, 95), breaks = seq(40, 90, 10)) +
    ggplot2::labs(title = "Oyun stili eşleşmesi", subtitle = "0–100 normalize profil; sağ taraf daha yüksek yoğunluk", x = "Stil skoru", y = NULL) +
    theme_kazanma() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_player_probability <- function(prediction, market = c("scorer", "card"), n = 8L) {
  market <- match.arg(market)
  value_col <- if (market == "scorer") "scorer_probability" else "card_probability"
  title <- if (market == "scorer") "Gol atma olasılığı" else "Kart görme olasılığı"
  subtitle <- if (market == "scorer") "Beklenen süre, gol/90 ve takım gol beklentisi" else "Beklenen süre, kart/90 ve rakip temas profili"

  df <- prediction$player_markets |>
    dplyr::arrange(dplyr::desc(.data[[value_col]])) |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(player = stats::reorder(player, .data[[value_col]]))

  ggplot2::ggplot(df, ggplot2::aes(.data[[value_col]], player, fill = team)) +
    ggplot2::geom_col(width = .62, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data[[value_col]], accuracy = 1)), hjust = -0.15, colour = "#EDF2EF", size = 3.5) +
    ggplot2::scale_fill_manual(values = setNames(c("#D7A84B", "#63B4A5"), c(prediction$home$team, prediction$away$team))) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, max(df[[value_col]]) * 1.22), expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Olasılık", y = NULL) +
    theme_kazanma() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_odds_comparison <- function(comparison) {
  df <- comparison |>
    dplyr::filter(
      active,
      supported,
      market_id %in% c("result", "ou_2_5", "btts")
    ) |>
    dplyr::transmute(
      option = paste(market, selection, sep = " · "),
      `Model` = model_probability,
      `Marjsız piyasa` = market_probability
    ) |>
    tidyr::pivot_longer(c("Model", "Marjsız piyasa"), names_to = "source", values_to = "probability") |>
    dplyr::mutate(option = stats::reorder(option, probability))

  ggplot2::ggplot(df, ggplot2::aes(probability, option, colour = source)) +
    ggplot2::geom_line(ggplot2::aes(group = option), colour = "#33423A", linewidth = 1) +
    ggplot2::geom_point(size = 3.3) +
    ggplot2::scale_colour_manual(values = c("Model" = "#D7A84B", "Marjsız piyasa" = "#63B4A5")) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, max(df$probability) * 1.14)) +
    ggplot2::labs(
      title = "Model — piyasa farkı",
      subtitle = "1X2, 2,5 gol ve karşılıklı gol; bahis marjı temizlenmiştir",
      x = "Olasılık", y = NULL
    ) +
    theme_kazanma() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
