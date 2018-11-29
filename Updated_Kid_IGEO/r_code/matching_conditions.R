library(pracma)
library(tidyverse)

cross <- pracma::cross

radius = 3.5

height_list = c(-1/sqrt(2), -1/2, 1/2, 1/sqrt(2))
view_angle = c(0.108, 0.073, -0.073, -0.108)

ac_curvature <- function(separation, height, t) {
  unit_height <- height_list[height]
  x_coord <- (1/10)*(separation + 1)
  y_coord <- -sqrt(1 - x_coord^2 - unit_height^2)
  left_pt <- radius*c(-x_coord, y_coord, unit_height)
  right_pt <- radius*c(x_coord, y_coord, unit_height)
  back_pt <- radius*c(0, cos(t), sin(t))
  vec_1 <- left_pt - back_pt
  vec_2 <- right_pt - back_pt
  normal <- pracma::cross(vec_1, vec_2)
  unit_normal <- normal/Norm(normal)
  plane_constant <- pracma::dot(left_pt, unit_normal)
  center_pt <- plane_constant*unit_normal
  to_left <- left_pt - center_pt
  to_right <- right_pt - center_pt
  perp_temp <- cross(cross(to_left, to_right), to_left)
  perp <- Norm(to_left)*perp_temp/Norm(perp_temp)
  end_angle <- acos(dot(to_left, to_right)/(Norm(to_left))^2)
  mid_angle <- end_angle/2
  max_height <- center_pt + cos(mid_angle)*to_left + sin(mid_angle)*perp
  return(abs(max_height[3] - radius*unit_height))
}

gc_curvature <- function(separation, height) {
  unit_height <- height_list[height]
  x_coord <- (1/10)*(separation + 1)
  y_coord <- -sqrt(1 - x_coord^2 - unit_height^2)
  left_pt <- radius*c(-x_coord, y_coord, unit_height)
  right_pt <- radius*c(x_coord, y_coord, unit_height)
  perp_temp <- cross(cross(left_pt, right_pt), left_pt)
  perp <- Norm(left_pt)*perp_temp/Norm(perp_temp)
  end_angle <- acos(dot(left_pt, right_pt)/(Norm(left_pt))^2)
  mid_angle <- end_angle/2
  max_height <- cos(mid_angle)*left_pt + sin(mid_angle)*perp
  return(abs(max_height[3] - radius*unit_height))
}

sep_1 <- c(-1.417, -1.189, 1.189, 1.417)
sep_2 <- c(-1.4, -1.186, 1.186, 1.4)
sep_3 <- c(-1.375, -1.183, 1.183, 1.375)
sep_4 <- c(-1.336, -1.177, 1.177, 1.336)
sep_5 <- c(-1.2735, -1.1681, 1.1681, 1.2735)

# Get all the data for GC Curvature

curve_data <- tibble::tibble(
  separation = rep(1:5, 4),
  height = rep(1:4, each = 5),
  geo_curve = purrr::map2(
    .x = separation,
    .y = height, 
    .f = function(x, y){round(gc_curvature(x, y), 4)}
    )
)

#RotationAngle = ViewAngle[[Height]];
#Turn = RotationTransform[-RotationAngle, {1, 0, 0}, {0, 0, 0}];
#mid_point_rot <- rot_matrix(size, separation, pull, spin) %*% mid_point_unrot
#highest_point <- abs(mid_point_rot[3])
#height <- abs(height_ac(size, separation, pull, spin))
#return(abs(highest_point - height)/length_ac(size, separation, pull))
