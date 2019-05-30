#' Degrees of Visual Angle Calculator
#'
#' Calculates degrees of visual angle from pixels, screen resolution, viewing distance, and monitor size
#' @param pixels size of the object in pixels
#' @param screen_res screen resolution, e.g. 1920 x 1080 = c(1920, 1080)
#' @param view_distance viewing distance in cm
#' @param monitor_dims width and height of the monitor in cm, e.g. c(50.92, 28.64)
#' @export

pixels_to_dva <- function(pixels, screen_res = c(1920,1080), view_distance = 60, monitor_dims = c(50.92, 28.64)){

  cm_per_pixel <- monitor_dims[1]/screen_res[1]

  size_in_cm <- pixels*cm_per_pixel

  dva_rad <- 2 * atan((size_in_cm/2)/view_distance)

  dva <- dva_rad * (180/pi)

  return(dva)

}

