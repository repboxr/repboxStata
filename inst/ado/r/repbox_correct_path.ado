// clear all

program repbox_correct_path, rclass
  args type orgpath default_ext supdir dodir
	mata: mata_correct_path("`type'", "`orgpath'", "`default_ext'", "`supdir'", "`dodir'", "`c(pwd)'")
	return local repbox_corrected_path = "`repbox_corrected_path'"
end


//set matastrict on
mata :

// type = "dir_exists","dir_create", "file_exists", "file_create"

string scalar function mata_correct_path(string scalar type, string scalar orgpath, string scalar default_ext, string scalar supdir, string scalar dodir, string scalar wdir) {
	string scalar result
	real scalar is_dir, exists, is_abs

	orgpath = normalize_path(orgpath)
	supdir = normalize_path(supdir)
	dodir = normalize_path(dodir)

	is_dir = type == "dir_exists" | type == "dir_create"
	exists = type == "dir_exists" | type == "file_exists"
	is_abs = pathisabs(orgpath)

	if (!is_dir) {
		orgpath = apply_default_extension(orgpath, default_ext)
	}

	if (is_abs) {
		// We generally want to enforce a path inside supdir
		// A path outside supdir is likely an error anyways
		orgpath = fit_abspath_to_supdir(orgpath, supdir)
	}

	// Path correction depends on:
	// 1. Whether it is a file or a directory
	// 2. The file or directory is supposed to exist (e.g. loading a file)
	//    or is created (e.g. saving a file or creating a directory)
	// 3. We have an absolute or relative path
	if (type == "file_exists" & is_abs) {
		result = correct_file_path_abs_exists(orgpath, supdir)
	}	else if (type == "file_exists" & !is_abs) {
		result = correct_file_path_rel_exists(orgpath, supdir, dodir, wdir)
	}	else if (type == "file_create" & is_abs) {
		result = correct_file_path_abs_create(orgpath, supdir)
	}	else if (type == "file_create" & !is_abs) {
		result = correct_file_path_rel_create(orgpath, supdir, dodir, wdir)
	} else if (type == "dir_exists" & is_abs) {
		result = correct_dir_path_abs_exists(orgpath, supdir)
	}	else if (type == "dir_exists" & !is_abs) {
		result = correct_dir_path_rel_exists(orgpath, supdir, dodir, wdir)
	}	else if (type == "dir_create" & is_abs) {
		result = correct_dir_path_abs_create(orgpath, supdir)
	}	else if (type == "dir_create" & !is_abs) {
		result = correct_dir_path_rel_create(orgpath, supdir, dodir, wdir)
	}

	// If no path could be found because file does not exist then
	// return the corresponding path if we would create the file or directory
	if (result == "" & exists) {
		if (is_dir) {
			result = mata_correct_path("dir_create", orgpath,default_ext, supdir,dodir,wdir)
		} else {
			result = mata_correct_path("file_create", orgpath,default_ext, supdir,dodir,wdir)
		}
	}
	// Store result as local variable that will be accessible in Stata
	st_local("repbox_corrected_path",result)
	return(result)
}

string scalar function apply_default_extension(string scalar path, string scalar default_ext) {
	string scalar ext
	ext = pathsuffix(path)
	if (ext != "") return(path)
	if (usubstr(default_ext,1,1)==".") {
		return(path + default_ext)
	} else {
		return(path + "." + default_ext)
	}
}

string scalar function fit_abspath_to_supdir(string scalar abspath, string scalar supdir) {
	if (is_subdir_or_same(supdir, abspath)) return(abspath)

	if (usubstr(abspath,1,1)=="/") {
		return(supdir + abspath)
	} else {
		return(supdir + "/" + subpath_right(abspath,-1))
	}
}

string scalar function correct_dir_path_abs_exists(string scalar orgpath, string scalar supdir) {
	string scalar basename
	string colvector matches
	real scalar best_i

	orgpath = normalize_path(orgpath)
	basename = subpath_right(orgpath, 1)
	matches = find_dir_all_recursive(basename, supdir)
	if (rows(matches)==0) return("")
	if (rows(matches)==1) return(matches[1])
	// The directory is found multiple times
	// Get that path that has longest match with orgpath from the right
	return(longest_path_match_from_right(orgpath, matches))
}

string scalar function correct_dir_path_rel_exists(string scalar orgpath, string scalar supdir, string scalar dodir, string scalar wdir) {
	string scalar abspath
	abspath = guess_best_rel_to_abs_path(orgpath, supdir, dodir, wdir)
	return(correct_dir_path_abs_exists(abspath, supdir))
}




string scalar function correct_file_path_abs_exists(string scalar orgpath, string scalar supdir) {
	string scalar basename
	string colvector matches
	real scalar best_i

	orgpath = normalize_path(orgpath)

	basename = pathbasename(orgpath)

	matches = find_file_all_recursive(basename, supdir)

	if (rows(matches)==0) return("")

	if (rows(matches)==1) return(matches[1])

	// The file is found multiple times
	// Get that file that has longest match with orgpath from the right
	return(longest_path_match_from_right(orgpath, matches))
}

string scalar function correct_file_path_rel_exists(string scalar orgpath, string scalar supdir, string scalar dodir, string scalar wdir) {
	string scalar abspath

	abspath = guess_best_rel_to_abs_path(orgpath, supdir, dodir, wdir)

	return(correct_file_path_abs_exists(abspath, supdir))
}

string scalar function correct_file_path_abs_create(string scalar orgpath, string scalar supdir) {
	string scalar basename, dirpath, mdirpath
	basename =  subpath_right(orgpath,1)
	dirpath = subpath_left(orgpath, -1)

	// Try to match directory to an existing sub directory in supdir
	mdirpath = correct_dir_path_abs_exists(dirpath, supdir)

	// If no match set directory to supdir
	if (mdirpath == "") mdirpath = supdir
	return(mdirpath + "/" + basename)
}

string scalar function correct_file_path_rel_create(string scalar orgpath, string scalar supdir, string scalar dodir, string scalar wdir) {
	string scalar basename, dirpath, mdirpath, absdirpath

	// previous code does not work if orgpath is just a file
	// basename =  subpath_right(orgpath,1)
  basename = pathbasename(orgpath)
	dirpath = subpath_left(orgpath, -1)

	// Try to match directory to an existing sub directory in supdir
	mdirpath = correct_dir_path_rel_exists(dirpath, supdir, dodir, wdir)

	// If no match set directory to supdir
	if (mdirpath == "") mdirpath = supdir
	return(mdirpath + "/" + basename)
}

string scalar function correct_dir_path_abs_create(string scalar orgpath, string scalar supdir) {
	// We can use the same procedure as for a file
	// What is the directory for a file is the parent directory for the dir
	return(correct_file_path_abs_create(orgpath, supdir))
}

string scalar function correct_dir_path_rel_create(string scalar orgpath, string scalar supdir, string scalar dodir, string scalar wdir) {
	// We can use the same procedure as for a file
	// What is the directory for a file is the parent directory for the dir
	return(correct_file_path_rel_create(orgpath, supdir, dodir, wdir))

}

string scalar function guess_best_rel_to_abs_path(string scalar orgpath, string scalar supdir, string scalar dodir, string scalar wdir) {
	string scalar relpath
	real scalar dotdots
	dotdots = 0


	while (usubstr(orgpath,1,2) == "..") {
			orgpath = subpath_right(orgpath, -1)
			dotdots = dotdots +1
	}

	if (dotdots > 0) {
		wdir = subpath_left(dodir, -dotdots)
		dodir = subpath_left(dodir, -dotdots)
	}


	if (orgpath != "" & orgpath != ".") {
		relpath = "/" + orgpath
	} else {
		relpath = ""
	}

	if (is_subdir_or_same(supdir, wdir)) {
		return(wdir + relpath)
	}

	if (is_subdir_or_same(supdir, dodir)) {
		return(wdir + relpath)
	}

	return(supdir + relpath)
}

real scalar function is_subdir_or_same(string scalar pdir, string scalar sdir) {
	real scalar plen, slen

	plen = ustrlen(pdir)
	slen = ustrlen(sdir)

	if (plen < slen) return(0)
	if (plen == slen) {
		if (pdir == sdir) return(1)
		return(0)
	}
	if (usubstr(sdir,1,plen)==pdir) return(1)
	return(0)

}

string scalar function normalize_path(string scalar path) {
	path = subinstr(path,"\", "/", .)
  path = subinstr(path,"//", "/", .)
	return(path)
}


real scalar function path_match_size_from_right(string scalar path1, string scalar path2) {
	real scalar size
	string scalar p1, p2

	size = 1
	while(1==1) {
		p1 = subpath_right(path1, size)
		p2 = subpath_right(path2, size)
		if (p1 != p2) return(size-1)
		if (p1 == path1 | p2 == path2) return(size)
		size = size+1
	}

}

string scalar function longest_path_match_from_right(string scalar path, string colvector candidates) {
	real scalar i, best_i, max_size, size

	max_size = 0
	best_i = 0
	for (i = 1; i <= rows(candidates); i++) {
		size = path_match_size_from_right(path, candidates[i])
		if (size > max_size) {
			max_size = size
			best_i = i
		}
	}
	if (best_i > 0) {
		return(candidates[best_i])
	}
	return("");
}

real scalar function count_characters(string scalar str, string scalar char) {
	ustrlen(str) - ustrlen(usubinstr(str, char, "", .))
}


string colvector function find_dir_all(string scalar dir, string colvector dirs)
{
	real scalar         i
	string colvector    res
	string scalar       fullname

	for (i=1; i<=rows(dirs); i++) {
		fullname = dirs[i] + "/" + dir
		if (direxists(fullname)) {
			res = res \ fullname
		}
	}
	return(res)
}


string colvector function find_dir_all_recursive(string scalar dir, string scalar pdir) {
	string colvector dirs
	dirs = list_dirs_recursive(pdir)
	return(find_dir_all(dir, dirs))
}


string colvector function find_file_all(string scalar file, string colvector dirs)
{
	real scalar         i
	string colvector    files
	string scalar       fullname

//	files = [];

	for (i=1; i<=rows(dirs); i++) {
		fullname = dirs[i] + "/" + file
		if (fileexists(fullname)) {
			files = files \ fullname
		}
	}
	return(files)
}


string colvector function find_file_all_recursive(string scalar file, string scalar pdir) {
	string colvector dirs
	// Get pdir and all descendant directories
	dirs = pdir \ list_dirs_recursive(pdir)
	return(find_file_all(file, dirs))
}


string colvector function list_dirs_recursive(string scalar pdir) {
	real scalar i
	string colvector dirs, all_dirs, child_dirs
	dirs = dir(pdir, "dirs", "*")
	for (i=1; i<=rows(dirs); i++) {
		dirs[i] =  pdir + "/" + dirs[i]
	}
	all_dirs = dirs

	for (i=1; i<=rows(dirs); i++) {
		child_dirs =  list_dirs_recursive(dirs[i])
		all_dirs = all_dirs \ child_dirs
	}
	//return(dirs[|1 \ 3|])

	return(all_dirs)
}

string scalar function subpath_right(string scalar path, real scalar sub_pos) {
	path = usubinstr(path,"\", "/",.)

	if (sub_pos==0) {
		return(path)
	}
	real scalar pos
	if (sub_pos < 0) {
		pos = ustrpos(path,"/")
		// no "/" found return empty string
		if (pos == 0) {
			return("")
		}
		if (sub_pos == -1) {
		 return(usubstr(path, pos+1,.))
		}

		// call recursively for sub_pos < -1
		return(subpath_right(usubstr(path, pos+1,.), sub_pos+1))
	}
	if (sub_pos == 1) {
		pos = ustrrpos(path,"/")
		// no "/" found return empty string
		if (pos == 0) {
			return("")
		}
		if (sub_pos == 1) {
		 return(usubstr(path, pos+1,.))
		}
	}

	// For sup_pos > 1 convert to negative sup_pos
	real scalar num_parts
	num_parts = 1+ustrlen(path) - ustrlen(usubinstr(path, "/", "", .))

	return(subpath_right(path, sub_pos-num_parts))
}

string scalar function subpath_left(string scalar path, real scalar sub_pos) {
	path = normalize_path(path)

	if (sub_pos==0) {
		return(path)
	}
	real scalar pos
	if (sub_pos < 0) {
		pos = ustrrpos(path,"/")
		// no "/" found return empty string
		if (pos == 0) {
			return("")
		}
		if (sub_pos == -1) {
		 return(usubstr(path,1, pos-1))
		}

		// call recursively for sub_pos < -1
		return(subpath_left(usubstr(path,1,pos-1), sub_pos+1))
	}
	if (sub_pos == 1) {
		pos = ustrpos(path,"/")
		// no "/" found return empty string
		if (pos == 0) {
			return("")
		}
		if (sub_pos == 1) {
		 return(usubstr(path,1, pos-1))
		}
	}

	// For sup_pos > 1 convert to negative sup_pos
	real scalar num_parts
	num_parts = 1+count_characters(path, "/")

	return(subpath_left(path, sub_pos-num_parts))
}


end


/*

// Debugging code

cd C:/libraries/repbox/projects_reg/ecta_84_5_8/mod

repbox_correct_path "file_exists" "LaFaveThomas.dta" ".dta" "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod" "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod"

mata: normalize_path("LaFaveThomas.dta")

mata: pathisabs("LaFaveThomas.dta")

mata: apply_default_extension("LaFaveThomas.dta", ".dta")


mata: correct_file_path_rel_exists("LaFaveThomas.dta", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod")

mata: guess_best_rel_to_abs_path("LaFaveThomas.dta", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod")

// C:/libraries/repbox/projects_reg/ecta_84_5_8/mod/LaFaveThomas.dta

mata: correct_file_path_abs_exists("C:/libraries/repbox/projects_reg/ecta_84_5_8/mod/LaFaveThomas.dta", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod")

// Steps in correct_file_path_abs_exists

/*
	orgpath = normalize_path(orgpath)

	basename = pathbasename(orgpath)

	matches = find_file_all_recursive(basename, supdir)

	if (rows(matches)==0) return("")

	if (rows(matches)==1) return(matches[1])

	// The file is found multiple times
	// Get that file that has longest match with orgpath from the right
	return(longest_path_match_from_right(orgpath, matches))
*/

mata: normalize_path("C:/libraries/repbox/projects_reg/ecta_84_5_8/mod/LaFaveThomas.dta")
// C:/libraries/repbox/projects_reg/ecta_84_5_8/mod/LaFaveThomas.dta

mata: pathbasename("C:/libraries/repbox/projects_reg/ecta_84_5_8/mod/LaFaveThomas.dta")

// LaFaveThomas.dta

mata: find_file_all_recursive("LaFaveThomas.dta", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod")

// Empty... but why? Let us look at find_file_all_recursive

mata: find_file_all("LaFaveThomas.dta", "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod")


repbox_correct_path "file_exists" "LaFaveThomas.dta" ".dta" "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod" "C:/libraries/repbox/projects_reg/ecta_84_5_8/mod"

display "`r(repbox_corrected_path)'"


display "`r(repbox_corrected_path)'"

mata: mata_correct_path("file_exists", "../data/interact",".do", "C:/libraries/repbox/projects_reg/testsupp","C:/libraries/repbox/projects_reg/testsupp/mod/code", "`c(pwd)'")

mata: mata_correct_path("file_exists", "code/mycode",".do", "C:/libraries/repbox/projects_reg/testsupp","C:/libraries/repbox/projects_reg/testsupp/mod/code", "`c(pwd)'")

mata: mata_correct_path("file_create", "C:/org/code/mycode",".do", "C:/libraries/repbox/projects_reg/testsupp","C:/libraries/repbox/projects_reg/testsupp/mod/code", "`c(pwd)'")

display "`repbox_corrected_path'"


mata: mata_correct_path("dir_exists", "code","", "C:/libraries/repbox/projects_reg/testsupp","C:/libraries/repbox/projects_reg/testsupp/mod/code", "`c(pwd)'")

display "`repbox_corrected_path'"

repbox_correct_path "file_create" "C:/org/code/mycode" ".do" "C:/libraries/repbox/projects_reg/testsupp" "C:/libraries/repbox/projects_reg/testsupp/mod/code"



display "`repbox_file_name'"

mata: correct_file_path_abs_exists("orgi/code/interact.do", "C:/libraries/repbox/projects_reg/testsupp")

mata: find_file_all_recursive("interact.do", "C:/libraries/repbox/projects_reg/testsupp")

mata: list_dirs_recursive("C:/libraries/repbox/projects_reg/testsupp")

mata: subpath_right("C:\mydir1\mydir2\mydir3\mydir4\myfile", -3)

mata: subpath_right("C:\mydir1\mydir2\mydir3\mydir4\myfile", 2)
*/
