theme_sm_2 <- function(background = 'white', title_col  = 'black', text_col = '#575757', title_font = 'Avignon-Bold-Regular', subfont = 'Open Sans', grid = 'Horizontal', title_size = 24, legend = TRUE, black = FALSE, hidden_y_tick = FALSE, hidden_x_tick = FALSE, x_angle = 0, y_angle = 0,legend_title = '') {
  if(black == F) {
    
    ret <- theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line = element_line(colour = background),
      axis.text.x = element_text(colour=text_col,angle=0,face="plain", family =subfont,size = round(title_size * 0.45,0)),
      axis.text.y = element_text(colour=text_col,angle=0,hjust=1,vjust=0,face="plain", family =subfont, size = round(title_size * 0.45,0)),
      axis.title.x = element_text(colour=text_col,angle=0,hjust=.5,vjust=0,face="plain", family =subfont,size = round(title_size * 0.5,0) ,margin = margin(10, 0, 0,0)),
      axis.title.y = element_text(colour=text_col,angle=90,hjust=.5,vjust=.5,face="plain", family =subfont,size = round(title_size * 0.5,0),margin = margin(0, 10, 0,0)),
      strip.text = element_text(colour=text_col, family =subfont,size = round(title_size * 0.35,0)),
      plot.subtitle=element_text(face="italic", color=text_col, family ='OpenSans-Italic', size = round(title_size * 0.7,0), margin = margin(-10, 5, 15,10)),
      plot.caption =element_text(face="italic", color=text_col, family ='OpenSans-Italic', size = round(title_size * 0.35,0)),
      panel.background=element_rect(fill=background,colour=background),
      text=element_text(family=title_font, face = 'bold', colour = title_col),
      plot.margin=unit(c(1,1,1,1),"cm"),
      plot.title = element_text(size=title_size, margin = margin(5, 10, 25,10)),
      plot.background = element_rect(fill = background,colour=background),
      panel.border = element_blank(),
    )
  } else {
    background <- '#1B1E23'
    text_col <- '#bababa'
    ret <- theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line = element_line(colour = '#1B1E23'),
      axis.text.x = element_text(colour=text_col,angle=0,face="plain", family =subfont,size = round(title_size * 0.45,0)),
      axis.text.y = element_text(colour=text_col,angle=0,hjust=1,vjust=0,face="plain", family =subfont, size = round(title_size * 0.45,0)),
      axis.title.x = element_text(colour=text_col,angle=0,hjust=.5,vjust=0,face="plain", family =subfont,size = round(title_size * 0.5,0) ,margin = margin(10, 0, 0,0)),
      axis.title.y = element_text(colour=text_col,angle=90,hjust=.5,vjust=.5,face="plain", family =subfont,size = round(title_size * 0.5,0),margin = margin(0, 10, 0,0)),
      strip.text = element_text(colour=text_col, family =subfont,size = round(title_size * 0.35,0)),
      plot.subtitle=element_text(face="italic", color="#f2f2f2", family ='OpenSans-Italic', size = round(title_size * 0.7,0), margin = margin(-10, 5, 15,10)),
      plot.caption =element_text(face="italic", color="#f2f2f2", family ='OpenSans-Italic', size = round(title_size * 0.35,0)),
      panel.background=element_rect(fill=background,colour=background),
      panel.border = element_blank(),
      text=element_text(family=title_font, face = 'bold', colour = 'white'),
      plot.margin=unit(c(1,1,1,1),"cm"),
      plot.title = element_text(size=title_size, margin = margin(5, 10, 25,10)),
      plot.background = element_rect(fill = background,colour=background)
    )
  }
  if(grid == 'Horizontal') {
    ret$panel.grid.major.y = element_line( size=.1, color="darkgrey", linetype = 'dashed')
  } else if(grid == 'Vertical') {
    ret$panel.grid.major.x = element_line( size=.1, color="darkgrey", linetype = 'dashed')
  } else {
    ret <- theme_minimal() +ret
  }
  if(legend == FALSE) {
    ret$legend.position="none"
  }
  if (hidden_y_tick == T) {
    ret$axis.text.y = element_blank()
  }
  if (hidden_x_tick == T) {
    ret$axis.text.x = element_blank()
  }
  if (x_angle != 0) {
    ret$axis.text.x$angle = x_angle
    ret$axis.text.x$hjust = 1
    ret$axis.text.x$vjust = 1
  }
  if (y_angle != 0) {
    ret$axis.text.y$angle = y_angle
    ret$axis.text.y$hjust = 1
    ret$axis.text.y$vjust = 1
  }
  return(theme_minimal() +  ret)
}
