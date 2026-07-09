# ============================================================
# analise_variaveis.R
# Análise automática das variáveis demográficas e acadêmicas
# ============================================================

gerar_grafico <- function(tab, info, periodo, pasta_gra){

  tabela_plot <- tab |>
    mutate(
      Periodo = formatar_periodo(`Periodo de Ingresso`),
      Curriculo = factor(
        `Curriculo Entrada`,
        levels = c(1999, 2017),
        labels = c("Currículo 1999", "Currículo 2017")
      )
    )

  # Paleta para Sexo
  if(info$coluna == "Sexo"){
    escala <- scale_fill_manual(
      values = c(
        "FEMININO" = "#E15759",
        "MASCULINO" = "#4E79A7",
        "Feminino" = "#E15759",
        "Masculino" = "#4E79A7"
      )
    )
  } else{
    escala <- scale_fill_manual(
      values = c(
        "#4E79A7",
        "#E15759",
        "#59A14F",
        "#F28E2B",
        "#B07AA1",
        "#76B7B2",
        "#EDC948",
        "#9C755F"
      )
    )
  }

  g <- ggplot(
    tabela_plot,
    aes(
      x = Periodo,
      y = Taxa,
      fill = .data[[info$coluna]]
    )
  ) +

    geom_col(
      position = "dodge",
      width = 0.72,
      colour = "black",
      linewidth = .20
    ) +

    geom_text(
      aes(label = sprintf("%.1f", Taxa)),
      position = position_dodge(width = .72),
      vjust = -.35,
      size = 3
    ) +

    facet_grid(. ~ Curriculo) +

    escala +

    scale_y_continuous(
      expand = expansion(mult = c(0, .10))
    ) +

    labs(
      title = paste0(
        "Taxa de evasão por ",
        info$titulo,
        " - ",
        periodo,
        "º período"
      ),
      x = "Período de ingresso",
      y = "Taxa de evasão (%)",
      fill = info$titulo
    ) +

    theme_classic(base_size = 13) +

    theme(
      plot.title = element_text(
        face = "bold",
        hjust = .5
      ),
      legend.position = "top",
      legend.title = element_text(face="bold"),
      strip.text = element_text(face="bold"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )

  ggsave(
    filename = file.path(
      pasta_gra,
      paste0(
        "figura_",
        info$arquivo,
        "_p",
        periodo,
        ".png"
      )
    ),
    plot = g,
    width = 14,
    height = 6,
    dpi = 300,
    bg = "white"
  )

}
