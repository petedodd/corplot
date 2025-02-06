##' A B&W smoothed contour corner plot
##'
##' TODO
##'
##' @title ppairs
##' @param D data.frame or similar
##' @param alph transparency
##' @return a ggplot2 object
##' @author Pete Dodd
##' @import ggplot2
##' @import ggpubr
##' @import GGally
##' @export
ppairs <- function(D,alph=0.5){
  GP <- GGally::ggpairs(D, switch = "both",
                lower = list(continuous = GGally::wrap("density",col="black",alpha=alph)),
                upper = "blank")+
    ggplot2::theme_bw() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(0.1, "lines"),
                   panel.spacing.y = ggplot2::unit(0.1, "lines"),
                   axis.text.x = ggplot2::element_text(angle=45,hjust=1),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.placement = "outside",
                   panel.border = ggplot2::element_rect(colour = "black")) +
    ggpubr::grids()
  GP
}
