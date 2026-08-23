# Chart map

| Section | Analytical question | Family / chart | Fields | Palette | QA surface |
|---|---|---|---|---|---|
| Maç merkezi | Hangi 90 dakika sonucu daha olası? | Comparison / vertical bar | result, probability | Gold / neutral / teal | Shiny panel + `work/chart-qa/outcomes.png` |
| Maç merkezi | Hangi kesin skorlar en fazla olasılık taşıyor? | Matrix / heatmap | home_goals, away_goals, probability | Deep green → gold | Shiny panel |
| Stil savaşı | Takımlar oyun profilinde nerede ayrışıyor? | Comparison / dumbbell | metric, team, value | Gold / teal + non-color position | Shiny panel + `work/chart-qa/styles.png` |
| Oyuncu radarları | En yüksek gol olasılığı kimde? | Ranking / horizontal bar | player, team, scorer_probability | Team colors + direct labels | Shiny panel |
| Oyuncu radarları | En yüksek kart olasılığı kimde? | Ranking / horizontal bar | player, team, card_probability | Team colors + direct labels | Shiny panel |

All probability charts expose their denominator as a 0–100% probability and keep exact labels. The style comparison is a normalized profile, not an absolute team-quality scale.
