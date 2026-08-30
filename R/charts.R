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

plot_top_scores <- function(prediction, n = 8L) {
  if (is.null(prediction$top_scores) || nrow(prediction$top_scores) == 0) return(NULL)
  df <- prediction$top_scores |>
    dplyr::slice_head(n = as.integer(n)) |>
    dplyr::mutate(
      score_label = paste0(score, "  (", scales::percent(probability, accuracy = .1), " · Oran: ", fair_odds, ")"),
      score = factor(score, levels = rev(score)),
      color_group = factor(outcome, levels = c("Ev Sahibi", "Beraberlik", "Deplasman"))
    )

  ggplot2::ggplot(df, ggplot2::aes(x = probability, y = score, fill = color_group)) +
    ggplot2::geom_col(width = 0.65, show.legend = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = score_label),
      hjust = -0.05,
      colour = "#F5F8F6",
      fontface = "bold",
      size = 3.6
    ) +
    ggplot2::scale_fill_manual(
      values = c("Ev Sahibi" = "#D7A84B", "Beraberlik" = "#68756E", "Deplasman" = "#63B4A5"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, max(df$probability) * 1.55), expand = c(0, 0)) +
    ggplot2::labs(
      title = "En Olası Kesin Skorlar Sıralaması",
      subtitle = "Poisson gol dağılımından türetilen kesin skor olasılıkları ve adil oranlar",
      x = "Skor Olasılığı",
      y = NULL
    ) +
    theme_kazanma() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_htft_probabilities <- function(prediction) {
  if (is.null(prediction$htft$htft_table) || nrow(prediction$htft$htft_table) == 0) return(NULL)
  df <- prediction$htft$htft_table |>
    dplyr::mutate(
      label_full = paste0(code, " (", label, ")"),
      label_full = factor(label_full, levels = rev(label_full)),
      pct_label = paste0(scales::percent(probability, accuracy = .1), "  (Oran: ", fair_odds, ")")
    )

  ggplot2::ggplot(df, ggplot2::aes(x = probability, y = label_full, fill = probability)) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_label),
      hjust = -0.05,
      colour = "#F5F8F6",
      fontface = "bold",
      size = 3.5
    ) +
    ggplot2::scale_fill_gradient(low = "#1F3B32", high = "#5FE3C6") +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, max(df$probability) * 1.55), expand = c(0, 0)) +
    ggplot2::labs(
      title = "İlk Yarı / Maç Sonu (İY/MS) Senaryo Olasılıkları",
      subtitle = "9 farklı İY/MS kombinasyonunun ortak olasılık dağılımı ve adil oranları",
      x = "Kombinasyon Olasılığı",
      y = NULL
    ) +
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

plot_team_learning_evolution <- function(learning_summary) {
  if (is.null(learning_summary) || nrow(learning_summary) == 0) return(NULL)
  
  df <- learning_summary |>
    dplyr::filter(matches_played > 0)
  if (nrow(df) == 0) {
    df <- learning_summary |> dplyr::slice_head(n = 10)
  }
  
  df_long <- dplyr::bind_rows(
    df |> dplyr::transmute(team = short, metric = "Hücum Gücü", base = base_attack, current = current_attack, delta = attack_delta),
    df |> dplyr::transmute(team = short, metric = "Savunma Gücü", base = base_defence, current = current_defence, delta = defence_delta)
  ) |>
    dplyr::mutate(
      team = factor(team, levels = rev(unique(df$short))),
      direction = dplyr::case_when(delta > 0 ~ "Arttı (+)", delta < 0 ~ "Azaldı (-)", TRUE ~ "Değişmedi")
    )

  ggplot2::ggplot(df_long, ggplot2::aes(y = team)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = base, xend = current, yend = team, colour = direction),
      linewidth = 1.2,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed")
    ) +
    ggplot2::geom_point(ggplot2::aes(x = base), colour = "#68756E", size = 2.8) +
    ggplot2::scale_colour_manual(
      values = c("Arttı (+)" = "#63B4A5", "Azaldı (-)" = "#E06A6A", "Değişmedi" = "#B8C4BD"),
      breaks = c("Arttı (+)", "Azaldı (-)", "Değişmedi"),
      drop = FALSE
    ) +
    ggplot2::facet_wrap(~ metric, scales = "free_x") +
    ggplot2::labs(
      title = "Biten Maçlardan Öğrenilen Takım Gücü Evrimi",
      subtitle = "Gri nokta: Başlangıç öncülü · Ok ve renkli nokta: Biten maçlar (xG / goller) sonrası güncellenen model puanı",
      x = "Güç Puanı (35 – 95)", y = NULL
    ) +
    theme_kazanma() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "top")
}

plot_exact_score_donut <- function(stats) {
  if (is.null(stats) || is.null(stats$total) || stats$total == 0) {
    df_empty <- tibble::tibble(category = "Veri Bekleniyor", count = 1, fraction = 1, ymax = 1, ymin = 0)
    return(
      ggplot2::ggplot(df_empty, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = category)) +
        ggplot2::geom_rect(fill = "#27332E") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(c(1, 4)) +
        ggplot2::annotate("text", x = 1, y = 0.5, label = "Henüz biten\nmaç yok", colour = "#94A3B8", size = 4.5, fontface = "bold") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#101815", colour = NA))
    )
  }

  exact_n <- stats$exact_hits %||% 0
  top3_n <- stats$top3_hits %||% 0
  miss_n <- max(0, (stats$total %||% 0) - exact_n - top3_n)
  total <- exact_n + top3_n + miss_n
  if (total == 0) total <- 1

  df <- tibble::tibble(
    category = factor(c("Tam Skor İsabeti", "Top-3 Skor Kapsama", "Farklı Skor (Tutmayan)"), levels = c("Tam Skor İsabeti", "Top-3 Skor Kapsama", "Farklı Skor (Tutmayan)")),
    count = c(exact_n, top3_n, miss_n),
    fraction = c(exact_n, top3_n, miss_n) / total,
    color = c("#5FE3C6", "#D7A84B", "#27332E")
  )
  df$ymax <- cumsum(df$fraction)
  df$ymin <- c(0, head(df$ymax, n = -1))

  pct_exact <- scales::percent(exact_n / total, accuracy = 0.1)
  pct_top3_total <- scales::percent((exact_n + top3_n) / total, accuracy = 0.1)

  ggplot2::ggplot(df, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = category)) +
    ggplot2::geom_rect(colour = "#101815", linewidth = 1.2) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0.8, 4.3)) +
    ggplot2::scale_fill_manual(values = c("Tam Skor İsabeti" = "#5FE3C6", "Top-3 Skor Kapsama" = "#D7A84B", "Farklı Skor (Tutmayan)" = "#27332E")) +
    ggplot2::annotate("text", x = 0.8, y = 0.5, label = paste0(pct_exact, "\nTam İsabet\n(Top-3: ", pct_top3_total, ")"), colour = "#F5F8F6", size = 4.2, fontface = "bold", lineheight = 0.95) +
    ggplot2::labs(
      title = "Kesin Skor İsabet Dağılımı",
      subtitle = paste0("Toplam ", total, " biten maç · Tam İsabet: ", exact_n, " · Top-3 Kapsama: ", exact_n + top3_n)
    ) +
    theme_kazanma() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(colour = "#C9D3CD", size = 10)
    )
}

plot_htft_donut <- function(stats) {
  if (is.null(stats) || is.null(stats$total) || stats$total == 0) {
    df_empty <- tibble::tibble(category = "Veri Bekleniyor", count = 1, fraction = 1, ymax = 1, ymin = 0)
    return(
      ggplot2::ggplot(df_empty, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = category)) +
        ggplot2::geom_rect(fill = "#27332E") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(c(1, 4)) +
        ggplot2::annotate("text", x = 1, y = 0.5, label = "Henüz biten\nmaç yok", colour = "#94A3B8", size = 4.5, fontface = "bold") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#101815", colour = NA))
    )
  }

  exact_n <- stats$htft_hits %||% 0
  ft_only_n <- stats$ft_only_hits %||% 0
  miss_n <- max(0, (stats$total %||% 0) - exact_n - ft_only_n)
  total <- exact_n + ft_only_n + miss_n
  if (total == 0) total <- 1

  df <- tibble::tibble(
    category = factor(c("İY/MS Tam İsabet", "MS Doğru (İY Yanlış)", "Tutmayan Kombinasyon"), levels = c("İY/MS Tam İsabet", "MS Doğru (İY Yanlış)", "Tutmayan Kombinasyon")),
    count = c(exact_n, ft_only_n, miss_n),
    fraction = c(exact_n, ft_only_n, miss_n) / total,
    color = c("#D7A84B", "#63B4A5", "#27332E")
  )
  df$ymax <- cumsum(df$fraction)
  df$ymin <- c(0, head(df$ymax, n = -1))

  pct_htft <- scales::percent(exact_n / total, accuracy = 0.1)
  pct_ft <- scales::percent((exact_n + ft_only_n) / total, accuracy = 0.1)

  ggplot2::ggplot(df, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = category)) +
    ggplot2::geom_rect(colour = "#101815", linewidth = 1.2) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0.8, 4.3)) +
    ggplot2::scale_fill_manual(values = c("İY/MS Tam İsabet" = "#D7A84B", "MS Doğru (İY Yanlış)" = "#63B4A5", "Tutmayan Kombinasyon" = "#27332E")) +
    ggplot2::annotate("text", x = 0.8, y = 0.5, label = paste0(pct_htft, "\nİY/MS İsabet\n(1X2: ", pct_ft, ")"), colour = "#F5F8F6", size = 4.2, fontface = "bold", lineheight = 0.95) +
    ggplot2::labs(
      title = "İlk Yarı / Maç Sonu (İY/MS) İsabet Dağılımı",
      subtitle = paste0("Toplam ", total, " biten maç · İY/MS İsabet: ", exact_n, " · 1X2 İsabet: ", exact_n + ft_only_n)
    ) +
    theme_kazanma() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(colour = "#C9D3CD", size = 10)
    )
}

