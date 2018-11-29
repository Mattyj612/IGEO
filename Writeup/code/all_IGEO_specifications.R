library(pracma)
library(Directional)
cross <- pracma::cross

# Size = {0, 1, 2, 3}
# Separation = {0, 1, ...., 6}
# Pull = {-5, ..., -1, 1, ... 5}
# Spin = {0, 1}

# -------- Geodesic Line --------------

radius <- function(size){
  return(2*2^(size/3))
  }

min_angle <- function(separation) {
  return(-pi/2 - (pi/18)*6^(separation/6))
  }
  
max_angle <- function(separation) {
  return(-pi/2 + (pi/18)*6^(separation/6))
  }

left_point <- function(size, separation){
  left_x <- cos(min_angle(separation))
  left_y <- sin(min_angle(separation))
  return(radius(size)*c(left_x, left_y, 0))
  }

right_point <- function(size, separation){
  right_x <- cos(max_angle(separation))
  right_y <- sin(max_angle(separation))
  return(radius(size)*c(right_x, right_y, 0))
  }

point_sep_rad <- function(separation){
  return(max_angle(separation) - min_angle(separation))
  }

length_gl <- function(size, separation){
  return(point_sep_rad(separation)*radius(size))
  }

height_gl <- 0

curvature_gl <- 0


# --------- Geodesic Curve -----------

back_point <- function(size, pull){
  back_y <- cos(pull*pi/15)
  back_z <- -sin(pull*pi/15)
  return(radius(size)*c(0, back_y, back_z))
  }

norm_vec <- function(size, separation, pull){
  left_pt <- left_point(size, separation)
  right_pt <- right_point(size, separation)
  back_pt <- back_point(size, pull)
  vec_1 <- (left_pt - back_pt)
  vec_2 <- (right_pt - back_pt)
  normal <- pracma::cross(vec_1, vec_2)
  return(normal/Norm(normal))
  }

rot_angle <- function(size, separation, pull, spin = NA_integer_){
  normal <- norm_vec(size, separation, pull)
  angle <- acos(dot(normal, c(0, 0, 1)))
  ifelse(is.na(spin), angle, 1.7*spin*angle)
  }

rot_matrix <- function(size, separation, pull, spin = NA_integer_){
  angle <- rot_angle(size, separation, pull, spin)
  return(rot.matrix(c(0, 0), theta = angle, rads = TRUE))
  }

height_gc <- function(size, separation, pull){
  rotated <- rot_matrix(size, separation, pull) %*% 
  left_point(size, separation)
  return(rotated[3])
  }

length_gc <- function(size, separation){
  return(point_sep_rad(separation)*radius(size))
}

mid_angle <- function(separation){
  return((min_angle(separation) + max_angle(separation))/2)
}

left_point <- function(size, separation){
  left_x <- cos(min_angle(separation))
  left_y <- sin(min_angle(separation))
  return(radius(size)*c(left_x, left_y, 0))
}

right_point <- function(size, separation){
  right_x <- cos(max_angle(separation))
  right_y <- sin(max_angle(separation))
  return(radius(size)*c(right_x, right_y, 0))
}

mid_point <- function(size, separation){
  mid_x <- cos(mid_angle(separation))
  mid_y <- sin(mid_angle(separation))
  return(radius(size)*c(mid_x, mid_y, 0))
}

highest_point <- function(size, separation, pull){
  rotated <- rot_matrix(size, separation, pull) %*%
    mid_point(size, separation)
  return(rotated[3])
}

curvature_gc <- function(size, separation, pull){
  highest_pt <- highest_point(size, separation, pull)
  height <- height_gc(size, separation, pull)
  return(abs(highest_pt - height)/length_gc(size, separation))
}
# -------- Arc Line ---------

chord_length <- function(size, pull){
  vector <- radius(size)*c(0, -1, 0) - back_point(size, pull)
  return(Norm(vector))
  }

height_al <-  function(size, pull){
  length <- radius(size)^2 - (chord_length(size, pull)/2)^2
  return(sign(pull)*sqrt(length))
  }

length_al <- function(size, separation, pull){
  new_radius <- chord_length(size, pull)/2
  return(new_radius*point_sep_rad(separation))
}

curvature_al <- 0

# ---------- Arc Curve ------------

height_ac <- function(size, separation, pull, spin){
  rotated <- rot_matrix(size, separation, pull, spin) %*% 
    left_point(size, separation)
  return(rotated[3])
}

length_ac <- function(size, separation, pull){
  left_pt <- left_point(size, separation)
  right_pt <- right_point(size, separation)
  norm_vec <- norm_vec(size, separation, pull)
  plane_constant <- dot(left_pt, norm_vec)
  center_pt <- plane_constant*norm_vec
  to_left <- left_pt - center_pt
  to_right <- right_pt - center_pt
  new_radius <- Norm(to_left)
  arc_length <- acos(dot(to_left, to_right)/(Norm(to_left))^2)
  return(new_radius*arc_length)
}

curvature_ac <- function(size, separation, pull, spin){
  left_pt <- left_point(size, separation)
  right_pt <- right_point(size, separation)
  norm_vec <- norm_vec(size, separation, pull)
  plane_constant <- dot(left_pt, norm_vec)
  center_pt <- plane_constant*norm_vec
  to_left <- left_pt - center_pt
  to_right <- right_pt - center_pt
  perp_temp <- cross(cross(to_left, to_right), to_left)
  perp <- Norm(to_left)*perp_temp/Norm(perp_temp)
  new_radius <- Norm(to_left)
  end_angle <- acos(dot(to_left, to_right)/(Norm(to_left))^2)
  mid_angle <- end_angle/2
  mid_point_unrot <- center_pt + cos(mid_angle)*to_left + sin(mid_angle)*perp
  mid_point_rot <- rot_matrix(size, separation, pull, spin) %*% mid_point_unrot
  highest_point <- abs(mid_point_rot[3])
  height <- abs(height_ac(size, separation, pull, spin))
  return(abs(highest_point - height)/length_ac(size, separation, pull))
}

