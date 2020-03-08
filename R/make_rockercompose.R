#' Create a Docker Compose file to build a Rocker image stack
#'
#' @param images_list A list of Rocker image data with variables for assembling
#'   Rocker Dockerfile templates, or a json or yaml file with this data
#' @param file_out the filename to write.  If NULL, the YAML is returned as
#'   character data
#' @param org the Docker orgaanizzation prefix for images
#' @param dockerfile_path The path where dockerfiles are stored, generally
#'   created with [make_rockerfiles()]
#'
#' @return the absolute path of file_out, or the Docker Compose YAML
#' @export
make_rockercompose <- function(images_list, file_out = "docker-compose.yml", org = "rocker",
                               dockerfile_path = "dockerfiles") {

  if (is.character(images_list) &&
    length(images_list) == 1 &&
    (file.exists(images_list) || grepl("^http", images_list))) {
    images_list <- yaml::read_yaml(images_list)
  }
  prefix <- file.path(dockerfile_path, "Dockerfile_")

  map_chr <- function(x, name) vapply(x, `[[`, character(1L), name)

  names <- map_chr(images_list, "ROCKER_IMAGE")
  tags <- map_chr(images_list, "ROCKER_TAG")

  dockerfiles <- paste0(prefix, names, "_", tags)
  names(dockerfiles) <- names

  image_name <- function(d, org) {
    x <- gsub(prefix, "", d)
    x <- gsub("_", ":", x)
    paste(org, x, sep = "/")
  }

  services <- lapply(dockerfiles, function(d) {
    list(
      image = image_name(d, org),
      build = list(
        context = ".",
        dockerfile = d
      )
    )
  })

  compose <- list(
    version = "3",
    services = services
  )

  yaml_out <- yaml::as.yaml(compose)

  if (is.null(file_out)) {
    return(yaml_out)
  } else {
    file <- normalizePath(file_out)
    cat(out, file = file_out)
    return(yaml_out)
  }
}
