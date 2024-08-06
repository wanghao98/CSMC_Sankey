#' @title Plot the Sankey plot in ggplot2
#'
#' @description This function generate the Sankey plot with options to save
#' the plot in user-defined directory.
#'
#' @param df a dataframe contains the variables Year for x axis, Value for Risk
#' Contributors, and RiskFactors for different groups.
#' @param title optional, to add title to the plot. Default is NULL

#' @return A ggplot object of the Sankey plot
#'
#' @examples
#' data(data_example)
#' sankey_test = Sankey_CSMC(data_example, title = "Example: Risk Factors for Stroke")
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr lead
#' @importFrom dplyr do
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @rdname sankey
#' @export

Sankey_CSMC <- function(df = df, title = NULL){
  #order data based on value for rectangle

  df_order <- df %>%
    group_by(Year) %>%
    arrange(Year,Value) %>%
    mutate(
      ymin = cumsum(lag(Value, default = 0)),
      ymax = ymin + Value,
      ymid = (ymin + ymax) / 2
    ) %>%
    ungroup()


  #create polygon data for connection across years
  polygon_data <- df_order %>%
    group_by(RiskFactor) %>%
    mutate(
      next_ymin = lead(ymin),
      next_ymax = lead(ymax),
      next_Year = lead(Year)
    ) %>%
    filter(!is.na(next_Year)) %>%
    ungroup()


  polygon_vertices <- polygon_data %>%
    rowwise() %>%
    do(data.frame(
      x = c(.$Year - 0.8, .$Year + 0.4, .$next_Year + 0.4, .$next_Year - 0.8),
      y = c(.$ymin, .$ymax, .$next_ymax, .$next_ymin),
      RiskFactor = .$RiskFactor,
      group = paste0(.$Year, "-", .$next_Year, "-", .$RiskFactor)
    ))

  sankey_plot =
    ggplot() +
    geom_polygon(data = polygon_vertices, aes(x = x, y = y, group = group, fill = RiskFactor), color = "black") +
    geom_rect(data = df_order, aes(xmin = Year - 0.8, xmax = Year + 1.5, ymin = ymin, ymax = ymax, fill = RiskFactor), color = "black") +
    geom_text(data = df_order, aes(x = Year, y = ymid, label = sprintf("%.2f", Value)), color = "white", size = 3) +
    scale_fill_manual(values = c(
      "Hypertension" = "#66c2a5",
      "Diabetes" = "#D75C20",
      "Smoking" = "#8da0cb",
      "Hypercholesterolemia" = "#CC7E4F",
      "Obesity" = "#433191"
    )) +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position="bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

    return(sankey_plot)


}
