#' A smarter rounding function
#'
#' This function rounds without truncating trailing zeros.
#' @param digits Number of decimal places to round to. Defaults to 1.
#' @export
smartRound <-
  function(x, digits=1) {
    if(digits < 1)
      stop("This is intended for the case digits >= 1.")
    if(length(digits) > 1) {
      digits <- digits[1]
      warning("Using only digits[1]")
    }
    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)
    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero
    tmp
  }



#' A ggplot2 theme for the Commission
#'
#' This function ensures all plots conform to the same standards
#' @param ticks Include axis ticks. Defaults to TRUE.
#' @param truncate Treat y axis as truncated (hide x axis line). Defaults to FALSE.
#' @param xTitle Include x axis title. Defaults to TRUE
#' @param yTitle Include y axis title. Defaults to TRUE
#' @export
theme_mhc <- function(base_size = 11, base_family = "VAG", ticks = TRUE, truncate = FALSE, xTitle = TRUE, yTitle = TRUE) {
  ret <- theme_bw(base_family = base_family, base_size = base_size) +
    theme(
      panel.border= element_blank(),
      axis.line.x = element_line(color="black", size = .6),
      axis.line.y = element_line(color="black", size = .6),
      panel.grid.major = element_line(colour = "grey50", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position="top",
      legend.title=element_blank(),
      legend.key=element_blank(),
      axis.ticks.length=unit(-0.25, "cm"),
      axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
      axis.text.y = element_text(margin=margin(10,10,10,10,"pt"))
      )
if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
}
  if (truncate) {
    ret <- ret + theme(axis.ticks.x = element_blank(),
                       axis.line.x = element_blank())
  }
  if (!xTitle) {
    ret <- ret + theme(axis.title.x = element_blank())
  }
  if (!yTitle) {
    ret <- ret + theme(axis.title.y = element_blank())
  }
  ret
}


#' Circular legend markers for ggplot line or bar charts
#'
#' This function removes standard item legends and adds circular legend items mapper to aes(color). Note that if aes(fill) is also mapped, 'guide=FALSE' must be added to to scale_fill_XX.
#' @param g A ggplot object. Defaults to g
#' @export
circularLegends <- function(g=g) {
  g <- g + geom_point(alpha = 0) +
    guides(colour = guide_legend(override.aes = list(size=4, linetype=0, alpha = 1)))
  g
}





