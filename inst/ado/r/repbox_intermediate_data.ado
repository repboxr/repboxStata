program repbox_intermediate_data, rclass
  args action corrected_path supdir intermediate_dir state_dir donum line counter

  if "`action'" == "" {
    local action "info"
  }

  if !inlist("`action'", "info", "archive_previous", "mark_saved") {
    di as error "repbox_intermediate_data: unknown action `action'"
    exit 198
  }

  mata: mata_repbox_intermediate_data("`corrected_path'", "`supdir'", "`intermediate_dir'", "`state_dir'")

  return local repbox_corrected_path = "`corrected_path'"
  return local repbox_rel_path = "`repbox_rel_path'"
  return local repbox_rel_dir = "`repbox_rel_dir'"
  return local repbox_file_base = "`repbox_file_base'"
  return local repbox_previous_counter = "`repbox_previous_counter'"
  return local repbox_next_counter = "`repbox_next_counter'"
  return local repbox_previous_state_file = "`repbox_previous_state_file'"
  return local repbox_new_state_file = "`repbox_new_state_file'"
  return local repbox_new_state_dir = "`repbox_new_state_dir'"
  return local repbox_previous_copy_path = "`repbox_previous_copy_path'"
  return local repbox_previous_copy_dir = "`repbox_previous_copy_dir'"
  return local repbox_new_copy_path = "`repbox_new_copy_path'"
  return local repbox_new_copy_dir = "`repbox_new_copy_dir'"

  if "`action'" == "archive_previous" {
    if `"`repbox_previous_copy_path'"' != "" {
      repbox_intermediate_mkdir `"`repbox_previous_copy_dir'"'
      copy `"`corrected_path'"' `"`repbox_previous_copy_path'"', replace
    }
    exit
  }

  if "`action'" == "mark_saved" {
    if `"`repbox_new_state_file'"' == "" {
      exit
    }
    repbox_intermediate_mkdir `"`repbox_new_state_dir'"'
    file open repbox_intermediate_state_out using `"`repbox_new_state_file'"', write replace
    file write repbox_intermediate_state_out `"`repbox_rel_path';`donum';`line';`counter'"' _n
    file close repbox_intermediate_state_out
    exit
  }
end

program repbox_intermediate_mkdir
  args dir

  local repbox_dir = subinstr(`"`dir'"', "\", "/", .)
  if `"`repbox_dir'"' == "" {
    exit
  }

  local repbox_make_dir ""
  local repbox_rest `"`repbox_dir'"'

  if substr(`"`repbox_rest'"', 1, 1) == "/" {
    local repbox_make_dir "/"
    local repbox_rest = substr(`"`repbox_rest'"', 2, .)
  }
  else if regexm(`"`repbox_rest'"', "^[A-Za-z]:/") {
    local repbox_make_dir = substr(`"`repbox_rest'"', 1, 2)
    local repbox_rest = substr(`"`repbox_rest'"', 4, .)
  }

  while `"`repbox_rest'"' != "" {
    if regexm(`"`repbox_rest'"', "^([^/]+)/(.+)$") {
      local repbox_part = regexs(1)
      local repbox_rest = regexs(2)
    }
    else {
      local repbox_part `"`repbox_rest'"'
      local repbox_rest ""
    }

    if `"`repbox_make_dir'"' == "" {
      local repbox_make_dir `"`repbox_part'"'
    }
    else if `"`repbox_make_dir'"' == "/" {
      local repbox_make_dir `"`repbox_make_dir'`repbox_part'"'
    }
    else {
      local repbox_make_dir `"`repbox_make_dir'/`repbox_part'"'
    }

    capture mkdir `"`repbox_make_dir'"'
  }
end

mata :

string scalar repbox_intermediate_normalize(string scalar path) {
  path = subinstr(path, "\", "/", .)
  while (ustrpos(path, "//") > 0) {
    path = subinstr(path, "//", "/", .)
  }
  return(path)
}

string scalar repbox_intermediate_join(string scalar left, string scalar right) {
  left = repbox_intermediate_normalize(left)
  right = repbox_intermediate_normalize(right)

  if (left == "") return(right)
  if (right == "") return(left)
  if (left == "/") return(left + right)
  if (usubstr(left, ustrlen(left), 1) == "/") return(left + right)
  return(left + "/" + right)
}

string scalar repbox_intermediate_dirname(string scalar path) {
  real scalar pos

  path = repbox_intermediate_normalize(path)
  if (path == "") return("")

  pos = ustrrpos(path, "/")
  if (pos == 0) return("")
  if (pos == 1) return("/")

  return(usubstr(path, 1, pos - 1))
}

string scalar repbox_intermediate_count_label(real scalar counter) {
  return(strofreal(counter, "%18.0g"))
}

void function mata_repbox_intermediate_data(
  string scalar corrected_path,
  string scalar supdir,
  string scalar intermediate_dir,
  string scalar state_dir
) {
  string scalar rel_path, rel_dir, file_base, prefix
  string scalar previous_state_file, new_state_file
  string scalar previous_copy_path, new_copy_path
  string scalar previous_copy_dir, new_copy_dir, new_state_dir
  string scalar test_file
  real scalar previous_counter, next_counter

  corrected_path = repbox_intermediate_normalize(corrected_path)
  supdir = repbox_intermediate_normalize(supdir)
  intermediate_dir = repbox_intermediate_normalize(intermediate_dir)
  state_dir = repbox_intermediate_normalize(state_dir)

  rel_path = ""
  rel_dir = ""
  file_base = ""
  previous_state_file = ""
  new_state_file = ""
  previous_copy_path = ""
  new_copy_path = ""
  previous_copy_dir = ""
  new_copy_dir = ""
  new_state_dir = ""
  previous_counter = 0
  next_counter = 1

  if (corrected_path != "") {
    prefix = ""
    if (supdir != "") {
      prefix = supdir + "/"
    }

    if (corrected_path == supdir) {
      rel_path = ""
    }
    else if (prefix != "" & ustrpos(corrected_path, prefix) == 1) {
      rel_path = usubstr(corrected_path, ustrlen(prefix) + 1, .)
    }
    else {
      rel_path = pathbasename(corrected_path)
    }

    file_base = pathbasename(rel_path)
    rel_dir = repbox_intermediate_dirname(rel_path)

    new_state_dir = repbox_intermediate_join(state_dir, rel_dir)
    new_copy_dir = repbox_intermediate_join(intermediate_dir, rel_dir)

    while (1) {
      test_file = repbox_intermediate_join(
        new_state_dir,
        "c" + repbox_intermediate_count_label(previous_counter + 1) + "_" + file_base + ".csv"
      )
      if (fileexists(test_file)) {
        previous_counter = previous_counter + 1
      }
      else {
        break
      }
    }

    next_counter = previous_counter + 1
    new_state_file = repbox_intermediate_join(
      new_state_dir,
      "c" + repbox_intermediate_count_label(next_counter) + "_" + file_base + ".csv"
    )
    new_copy_path = repbox_intermediate_join(
      new_copy_dir,
      "c" + repbox_intermediate_count_label(next_counter) + "_" + file_base
    )

    if (previous_counter > 0) {
      previous_state_file = repbox_intermediate_join(
        new_state_dir,
        "c" + repbox_intermediate_count_label(previous_counter) + "_" + file_base + ".csv"
      )
      previous_copy_dir = new_copy_dir
      previous_copy_path = repbox_intermediate_join(
        new_copy_dir,
        "c" + repbox_intermediate_count_label(previous_counter) + "_" + file_base
      )
    }
  }

  st_local("repbox_rel_path", rel_path)
  st_local("repbox_rel_dir", rel_dir)
  st_local("repbox_file_base", file_base)
  st_local("repbox_previous_counter", repbox_intermediate_count_label(previous_counter))
  st_local("repbox_next_counter", repbox_intermediate_count_label(next_counter))
  st_local("repbox_previous_state_file", previous_state_file)
  st_local("repbox_new_state_file", new_state_file)
  st_local("repbox_new_state_dir", new_state_dir)
  st_local("repbox_previous_copy_path", previous_copy_path)
  st_local("repbox_previous_copy_dir", previous_copy_dir)
  st_local("repbox_new_copy_path", new_copy_path)
  st_local("repbox_new_copy_dir", new_copy_dir)
}

end
