#' Create a palette of colours for a graph
#'
#' @param colour A vector containing the graph objects which require different colours
#'
#' @return A vector with HEX code colours for a graph.
#' These colours are created by PHS to be accessible when used in this order.
#' @export
#'
#' @examples
#' create_palette(c("2021/22", "2022/23", "2023/24"))
#' # [1] "#12436D" "#28A197" "#801650"

create_palette <- function(colour) {

  vector <- as.character(colour)

  if (length(unique(vector)) ==1 ) {
    palette <- "#12436D"

  } else if (length(unique(vector)) ==2) {
    palette <- c("#12436D", "#28A197")

  } else if (length(unique(vector)) ==3) {
    palette <- c("#12436D", "#28A197", "#801650")

  } else if (length(unique(vector)) ==4) {
    palette <- c("#12436D", "#28A197", "#801650", "#F46A25")

  } else if (length(unique(vector)) ==5) {
    palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3F085C")


  } else if (length(unique(vector)) ==6) {
    palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3F085C", "#3E8ECC")

  } else if (length(unique(vector)) ==7) {
    palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3F085C", "#3E8ECC", "#3D3D3D")


  } else if (length(unique(vector)) ==8) {
    palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3F085C", "#3E8ECC", "#3D3D3D", "#A285D1")

  } else {

    palette <- c("#12436D", "#94AABD", "#28A197", "#B4DEDB", "#801650", "#CCA2B9",
                 "#F46A25", "#FBC3A8", "#3D3D3D", "#A8A8A8", "#3E8ECC", "#A8CCE8",
                 "#3F085C", "#A285D1")
  }

  return(palette)

}
