radius = 3

height_list = c(-1/sqrt(2), -1/2, 1/2, 1/sqrt(2))

unit_height <- function(height) {
  height_list[height]
}

# line_height <- function(height){
#   radius*unit_height(height)
# }

x_separation <- function(separation){
  (3/32)*separation + 1/32
}

left_pt <- function(separation, height){
  x_coord <- x_separation(separation)
  z_coord <- unit_height(height)
  y_coord <- sqrt(1 - x_coord^2 - z_coord^2)
  left_pt <- radius*c(-x_coord, y_coord, z_coord)
  return(left_pt)
}

right_pt <- function(separation, height){
  x_coord <- radius*x_separation(separation)
  z_coord <- line_height(height)
  y_coord <- sqrt(radius^2 - x_coord^2 - z_coord^2)
  left_pt <- c(x_coord, y_coord, z_coord)
  return(right_pt)
}
