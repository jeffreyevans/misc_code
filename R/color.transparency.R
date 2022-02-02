# color = color name
# percent = % transparency
# name = an optional name for the color
transparency <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255, alpha = (100 - percent) * 255 / 100,
               names = name)
  return(t.col)
}