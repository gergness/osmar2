#' @include as-osmar.R
{}



#' Convert sp object to an osmar object
#'
#' Functions to convert a given \code{\link[sp]{sp}} object to osmar
#' infrastructure and objects.
#'
#' @param obj A \code{\link[sp]{Spatial}} object
#' @param ... Ignored
#'
#' @return A \code{\link{bbox}} object
#'
#'
#' @family as_osmar_bbox
#' @rdname as_osmar_sp
#'
#' @examples
#'   data("muc", package = "osmar2")
#'   muc_points <- as_sp(muc, "points")
#'   bbox(muc_points)           # sp::bbox object
#'   as_osmar_bbox(muc_points)  # osmar::bbox object
#'
#' @export
as_osmar_bbox.Spatial <- function(obj, ...) {
  stopifnot(requireNamespace("sp"))
  bb <- sp::bbox(obj)
  corner_bbox(left = bb[1,1], bottom = bb[2,1],
              right = bb[1,2], top = bb[2,2])
}

