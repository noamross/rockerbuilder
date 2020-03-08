#' Create a Rocker-style Dockerfile from a list of variables
#'
#' @param variables a list of variables for assembling Dockerfiles from Rocker
#'   whisker templates and partialss
#' @param file_out the file to write the Dockerfile to. If NULL, returns as
#'   character
#' @param ref the git reference of the source repository to use
#' @param dev_workflow If "1" builds Dockerfiles using the developer workflow
#'   for faster iteration. If "0", uses the normal workflow that places all
#'   rocker scripts in the base image.
#' @param partials_base_url The base URL of the source of partials, e.g.
#'   `"https://raw.githubusercontent.com/user/repo/`
#'
#' @return the absolute path of the Dockerfile, or its contents
#' @export
#'
make_rockerfile <-
  function(variables, file_out = NULL, ref = "master",
           dev_workflow = Sys.getenv("ROCKER_DEV_WORKFLOW", "0"),
           partials_base_url = "https://raw.githubusercontent.com/noamross/rocker-versioned2/") {
    if (is.null(variables[["PARTIAL_DOCKERFILES"]])) {
      stop("No PARTIAL_DOCKERFILES field in variables)")
    }

    if (dev_workflow == "1") variables$ROCKER_DEV_WORKFLOW <- 1

    partials <- c("start", variables[["PARTIAL_DOCKERFILES"]], "end")
    partials <- paste0(
      partials_base_url, "/", ref, "/partials/Dockerfile.", partials, ".partial"
    )
    template <- paste(unlist(lapply(partials, readLines)), collapse = "\n")
    dockerfile_text <- whisker::whisker.render(template, data = variables)
    if (is.null(file)) {
      return(dockerfile_text)
    } else {
      file <- normalizePath(file)
      cat(dockerfile_text, file = file)
      return(file)
    }
  }

#' Create Rocker-style dockerfiles from a list of lists of variables or JSON/YAML data
#'
#' @param images_list A list of lists of variables for assembling Rocker whisker
#'   templates, or a file or url of JSON/YAML data
#' @param out_dir the directory to place the Dockerfiles
#' @param ... other arguments passed to make_rockerfile
#'
#' @return a vector of Dockerfile file paths
#' @export
#'
#' @examples
make_rockerfiles <- function(images_list, out_dir = ".", ...) {
  if (is.character(images_list) &&
      length(images_list) == 1 &&
      (file.exists(images_list) || grepl("^http", images_list))) {
    images_list <- yaml::read_yaml(images_list)
  }

  dockerfiles <- lapply(images_list, function(z) {
    make_rockerfile(
      variables = z,
      file_out = file.path(
        out_dir,
        paste0("Dockerfile_", z[["ROCKER_IMAGE"]], "_", z[["ROCKER_TAG"]])
      ),
      ...
    )
  })
  return(unlist(dockerfiles))
}
