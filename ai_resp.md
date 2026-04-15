I checked the failure path. The bug is in `repbox_correct_path.ado`: once the exact-case lookup fails, the helper falls through to the “create path” logic, which is why your log ends up with `/mod/data.DTA` instead of the existing file. The patch below keeps exact-case lookup first, and only if that finds nothing does it scan directory entries case-insensitively. I also made the tie-break path comparison case-insensitive so the fallback can still choose the closest match from the right. For the case-folding, I used `ustrlower()`, which Stata documents as the Unicode-aware lowercase function; the path helpers such as `pathbasename()` are also part of the documented path utilities. ([Stata][1])

!MODIFICATION find_dir_all in repbox_correct_path.ado
scope = "function"
file = "repbox_correct_path.ado"
function_name = "find_dir_all"
description = "Keep exact-case directory matching first, and only if nothing matches fall back to case-insensitive directory-name matching while preserving the actual on-disk casing."
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```stata
string colvector function find_dir_all(string scalar dir, string colvector dirs)
{
  real scalar i, j
  string colvector exact_matches, ci_matches, entries
  string scalar fullname, target, entry

  exact_matches = J(0, 1, "")
  for (i = 1; i <= rows(dirs); i++) {
    fullname = dirs[i] + "/" + dir
    if (direxists(fullname)) {
      exact_matches = exact_matches \ fullname
    }
  }
  if (rows(exact_matches) > 0) {
    return(exact_matches)
  }

  ci_matches = J(0, 1, "")
  target = ustrlower(dir)
  for (i = 1; i <= rows(dirs); i++) {
    entries = dir(dirs[i], "dirs", "*")
    for (j = 1; j <= rows(entries); j++) {
      entry = entries[j]
      if (ustrlower(entry) == target) {
        ci_matches = ci_matches \ (dirs[i] + "/" + entry)
      }
    }
  }
  return(ci_matches)
}
```

!END_MODIFICATION find_dir_all in repbox_correct_path.ado

!MODIFICATION find_file_all in repbox_correct_path.ado
scope = "function"
file = "repbox_correct_path.ado"
function_name = "find_file_all"
description = "Keep exact-case file matching first, and only if nothing matches fall back to case-insensitive filename matching while returning the real filesystem casing."
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```stata
string colvector function find_file_all(string scalar file, string colvector dirs)
{
  real scalar i, j
  string colvector exact_matches, ci_matches, entries
  string scalar fullname, target, entry

  exact_matches = J(0, 1, "")
  for (i = 1; i <= rows(dirs); i++) {
    fullname = dirs[i] + "/" + file
    if (fileexists(fullname)) {
      exact_matches = exact_matches \ fullname
    }
  }
  if (rows(exact_matches) > 0) {
    return(exact_matches)
  }

  ci_matches = J(0, 1, "")
  target = ustrlower(file)
  for (i = 1; i <= rows(dirs); i++) {
    entries = dir(dirs[i], "files", "*")
    for (j = 1; j <= rows(entries); j++) {
      entry = entries[j]
      if (ustrlower(entry) == target) {
        ci_matches = ci_matches \ (dirs[i] + "/" + entry)
      }
    }
  }
  return(ci_matches)
}
```

!END_MODIFICATION find_file_all in repbox_correct_path.ado

!MODIFICATION path_match_size_from_right in repbox_correct_path.ado
scope = "function"
file = "repbox_correct_path.ado"
function_name = "path_match_size_from_right"
description = "Compare path suffixes case-insensitively so the fallback case-agnostic search can still choose the closest candidate from the right."
----------------------------------------------------------------------------------------------------------------------------------------------------

```stata
real scalar function path_match_size_from_right(string scalar path1, string scalar path2) {
  real scalar size
  string scalar p1, p2

  path1 = ustrlower(normalize_path(path1))
  path2 = ustrlower(normalize_path(path2))

  size = 1
  while (1 == 1) {
    p1 = subpath_right(path1, size)
    p2 = subpath_right(path2, size)
    if (p1 != p2) return(size - 1)
    if (p1 == path1 | p2 == path2) return(size)
    size = size + 1
  }
}
```

!END_MODIFICATION path_match_size_from_right in repbox_correct_path.ado

This should fix your Linux example without changing behavior when an exact-case file already exists.

[1]: https://www.stata.com/manuals/m-5ustrupper.pdf?utm_source=chatgpt.com "ustrupper( ) — Convert Unicode string to uppercase, ..."
