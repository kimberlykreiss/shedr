shed_style <- function(){

  ggplot2::theme(

    #Text format:
    ##Set the font, size, type, color of the title

    plot.title = ggplot2::element_text(size=18,
                                       hjust = 0),


    plot.caption = ggplot2::element_text(size=10,
                                         hjust = 0),

    #Axis format

    axis.ticks = ggplot2::element_blank(),

    axis.text = ggplot2::element_text(size=14),

    axis.title.y = ggplot2::element_blank(),

    #Grid lines

    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),

    #Panel background
    panel.background = ggplot2::element_blank()

  )
}
