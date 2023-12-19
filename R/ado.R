example = function() {
  ado_dir = "C:/libraries/repbox/ado/plus"
  are_repbox_ado_files_installed(ado_dir)
  copy_repbox_ado_files(ado_dir)
}

copy_repbox_ado_files = function(ado_dir) {
  if (!dir.exists(ado_dir)) stop("The specified ado directory ", ado_dir, " does not exist.")

  source_dir = system.file("ado", package="repboxStata")
  ado_files = list_repbox_ado_files(full.names = FALSE)

  source_files = file.path(source_dir, ado_files)
  dest_files = file.path(ado_dir, ado_files)


}

list_repbox_ado_files = function(full.names=FALSE, only_required=FALSE) {
  source_dir = system.file("ado", package="repboxStata")
  files = list.files(source_dir, "*.ado",full.names = full.names, recursive = TRUE)
  if (only_required) {
    files = files[has.substr(files, "repbox_")]
  }
  files
}

are_repbox_ado_files_installed = function(ado_dirs, not_found = c("error","msg","nothing")[1]) {
  ado_files = list_repbox_ado_files(full.names = FALSE)
  exists = rep(FALSE, length(ado_files))
  for (dir in ado_dirs) {
    dest_files = file.path(dir, ado_files)
    exists = exists | file.exists(dest_files)
  }
  if (!all(exists)) {
    msg = paste0("\nNot all repbox specific Stata ado files can be found in your specified ado directories:\n\n  ", paste0(ado_dir, collapse="\n  "), "\n\nPlease call copy_repbox_ado_files(ado_dir) to copy the required ado files.")
    if (not_found=="error") stop(msg)
    if (not_found=="msg") cat(msg)
    return(FALSE)
  }
  return(TRUE)
}

get_ado_dirs = function(default = "~/ado/plus") {
  res = getOption("repbox.stata.paths")$ado_dirs
  if (is.null(res)) return(default)
  return(res)
}

get_base_ado_dir = function(default = "/usr/local/stata/ado/base") {
  res = getOption("repbox.stata.paths")$base_ado_dir
  if (is.null(res)) return(default)
  return(res)
}
