##' A B&W smoothed contour corner plot
##'
##' TODO
##'
##' @title ppairs
##' @param D data.frame or similar
##' @param alph transparency
##' @param rotate_parms  Rotate parameter names: "xy" = both (default);
##' "x" = x-axis only; "y" = y-axis only; anything else = no rotation
##' @param group Optional name of a column in \code{D} to split/colour by
##' (e.g. a factor distinguishing "prior" vs "posterior" samples). When
##' \code{NULL} (default), behaviour is unchanged from the single-colour B&W plot.
##' @param colours Optional named or unnamed vector of colours to use for the
##' levels of \code{group}, passed to \code{scale_colour_manual}/\code{scale_fill_manual}.
##' Ignored if \code{group} is \code{NULL}.
##' @param legend Logical: if \code{group} is used, show a combined legend
##' (default \code{TRUE})?
##' @return a ggplot2 object
##' @author Pete Dodd
##' @import ggplot2
##' @import ggpubr
##' @import GGally
##' @export
ppairs <- function(D, alph = 0.5, rotate_parms = "xy",
                    group = NULL, colours = NULL, legend = TRUE) {
  if (!is.null(group)) {
    if (!group %in% names(D)) {
      stop("`group` must be the name of a column in D!")
    }
    plotcols <- setdiff(names(D), group)
    grpmapping <- ggplot2::aes(colour = .data[[group]])
    lowerspec <- GGally::wrap("density", alpha = alph)
    diagspec <- GGally::wrap("densityDiag", alpha = alph)
    lgnd <- if (isTRUE(legend) && length(plotcols) >= 2) c(2, 1) else NULL
  } else {
    plotcols <- names(D)
    grpmapping <- NULL
    lowerspec <- GGally::wrap("density", col = "black", alpha = alph)
    diagspec <- "densityDiag"
    lgnd <- NULL
  }
  GP <- GGally::ggpairs(D,
    columns = plotcols,
    mapping = grpmapping,
    switch = "both",
    lower = list(continuous = lowerspec),
    diag = list(continuous = diagspec),
    upper = "blank",
    legend = lgnd
  ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.spacing.x = ggplot2::unit(0.1, "lines"),
      panel.spacing.y = ggplot2::unit(0.1, "lines"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      panel.border = ggplot2::element_rect(colour = "black")
    ) +
    ggpubr::grids()
  if (rotate_parms == "xy") {
    GP <- GP +
      ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 45))
    GP <- GP +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 45))
  } else if (rotate_parms == "x") {
    GP <- GP +
      ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 45))
  } else if (rotate_parms == "y") {
    GP <- GP +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 45))
  }
  if (!is.null(group)) {
    if (!is.null(colours)) {
      GP <- GP +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::scale_fill_manual(values = colours)
    }
    if (isTRUE(legend)) {
      GP <- GP +
        ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
    }
  }
  GP
}


