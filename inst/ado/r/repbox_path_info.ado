program repbox_path_info, rclass
  args corrected_path supdir
  local old_corrected_path `"`r(repbox_corrected_path)'"'
  mata: mata_repbox_path_info("`corrected_path'", "`supdir'")
  // return repbox_corrected_path again so that it is still available
  // in r()
  return local repbox_corrected_path `"`old_corrected_path'"'
  return local repbox_rel_path = "`repbox_rel_path'"
  return local repbox_rel_dir = "`repbox_rel_dir'"
  return local repbox_file_base = "`repbox_file_base'"
end

mata :

string scalar function repbox_path_info_normalize(string scalar path) {
  path = subinstr(path, "\", "/", .)
  path = subinstr(path, "//", "/", .)
  return(path)
}

string scalar function repbox_path_info_subpath_left(string scalar path, real scalar sub_pos) {
  real scalar pos

  path = repbox_path_info_normalize(path)

  if (path == "") return("")
  if (sub_pos == 0) return(path)

  if (sub_pos < 0) {
    pos = ustrrpos(path, "/")
    if (pos == 0) return("")
    if (sub_pos == -1) {
      return(usubstr(path, 1, pos - 1))
    }
    return(repbox_path_info_subpath_left(usubstr(path, 1, pos - 1), sub_pos + 1))
  }

  pos = ustrpos(path, "/")
  if (pos == 0) return("")
  if (sub_pos == 1) {
    return(usubstr(path, 1, pos - 1))
  }

  return("")
}

void function mata_repbox_path_info(string scalar corrected_path, string scalar supdir) {
  string scalar rel_path, rel_dir, file_base, prefix

  corrected_path = repbox_path_info_normalize(corrected_path)
  supdir = repbox_path_info_normalize(supdir)

  rel_path = ""
  rel_dir = ""
  file_base = ""

  if (corrected_path == "") {
    st_local("repbox_rel_path", rel_path)
    st_local("repbox_rel_dir", rel_dir)
    st_local("repbox_file_base", file_base)
    return
  }

  prefix = supdir + "/"

  if (corrected_path == supdir) {
    rel_path = ""
  }
  else if (ustrpos(corrected_path, prefix) == 1) {
    rel_path = usubstr(corrected_path, ustrlen(prefix) + 1, .)
  }
  else {
    rel_path = pathbasename(corrected_path)
  }

  file_base = pathbasename(rel_path)

  if (rel_path != "" & ustrpos(rel_path, "/") > 0) {
    rel_dir = repbox_path_info_subpath_left(rel_path, -1)
  }
  else {
    rel_dir = ""
  }

  st_local("repbox_rel_path", rel_path)
  st_local("repbox_rel_dir", rel_dir)
  st_local("repbox_file_base", file_base)
}

end
