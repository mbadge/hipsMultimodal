# Read and view radiographs

#' Read a numpy image file from disk
#'
#' Uses the \code{reticulate} package to create an R representation of the numpy module
#' Created for issue-60
#'
#' \code{readNpy} takes a numpy file path
#'
#' \code{readImg} takes and image id and infers the path
#'
#' \code{showImg} is fxn composition of \code{readImg} and \code{vizR::Viz}
#'
#'
#' @param fp chr(1) file path to numpy image
#' @return array
#'
#' @family img
#' @export
#'
#' @examples
#' readNpy("/media/marcus/Vulcan/radiology/msh_hip/imgs/1104.15.1202.129.1155.118.20151106131219.124.npy")
readNpy <- function(fp) {
    stopifnot(file.exists(fp))

    np <- reticulate::import("numpy")
    img_arr <- np$load(fp)
    return(img_arr)
}

#' @rdname readNpy
#' @export
#' @examples
#' hipsCohort() %>% `[[`(1, "img") %>% readImg() %>% Viz()
readImg <- function(img_id, dir = hipsOpt(dir_img_proc)) {
    stopifnot(is.character(img_id))

    fp <- file.path(dir, paste(img_id, "npy", sep = "."))
    readNpy(fp)
}

#' @rdname readNpy
#' @export
#' @examples
#' hipsCohort() %>% `[[`(1, "img") %>% showImg()
showImg <- function(img_id, dir = hipsOpt(dir_img_proc)) {
    stopifnot(is.character(img_id))

    img <- readImg(img_id, dir)
    vizR::Viz(img)
}
