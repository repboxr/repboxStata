## Thoughts and strategies about path correction

Do files contain many paths to files or directories. To automatically run the do files, we need to correct them.

### Some examples of file paths

Here are some examples of possible file paths in a do file:

1. Absolute path

```
use C:\research\myarticle\data\mydata.dta
```

2. Relative path. Omit `.dta` file extension for dta file. Quote path

```
use "..\data\mydata"
```

3. Store path first in a global or local variable

```
global datafile = "C:\research\myarticle\data\mydata.dta"
use "$datafile"
```

4. Construct path via string splicng that combines a variable and constant (as variation now a local variable where splicing does not use a $, but a `'. We also have a Linux path)

```
local datadir = "~/research/myarticle/data"
use "`datadir'/mydata.dta"
```

or a possibly tougher version

```
local datadir = "~/research/myarticle/data/"
use "`datadir'mydata.dta"
```

5. More complex version with string splicing inside a for loop

```
local path = "C:\research\myarticle\data"
foreach file in myfiles {
  use "`path'/`file'"
}
```

### A dynamic path correction method at run time

Given the possibilities of string splicing with variables, relative paths and files that might be first generated in a script before being loaded again later, an effective static path correction approach at parse time may be hard to implement.

Therefore we use a dynamic path correction method at runtime using appropriate code transformation. Let us look at an example. The original code line that loads a data set is:

```
use $data, clear
```

The expression `$data` refers to an earlier defined global variable that contains the path of the `.dta` file that shall be loaded. We transform this code into the following:

```
rcall vanilla: ///
  source("~/statabox/supp/aejmic_11_2_7/repbox/find_files.R"); ///
  repbox_found_file = ///
    find.path("$data", "~/statabox/supp/aejmic_11_2_7", "use", "`c(pwd)'")
use "`r(repbox_found_file)'", clear
```

Since I don't know Stata well, I have written the dynamic path mapping code in R. It is the function `find.path` in an R script `find_files.R` that has been automatically copied into the repbox directory of this project. We call this R function using the Stata package [https://github.com/haghish/rcall](rcall). It stores the assigned variable `repbox_found_file` under `r(repbo_found_file)`. This variable now replaces the original file path argument `$data` of the `use` command.

Note that `rcall` first performs string splicing before passing arguments to R. This means the first argument of `find.path`, `"$data"` will actually contain the value of `$data` as a string, e.g. "./data/mydata.dta".  We also pass the project directory `"~/statabox/supp/aejmic_11_2_7"`, the command name `"use"` and the current Stata working directory. All this information should help to find the actual file path in the R function.

Basically in the R code we can list all existing files / directories in the supplement and match the provided path. Some points:

- Certain commands use default file extensions. E.g. `use` assumes that we have a `.dta` file even if the file extension is not provided. This means if the command is `use` our R code should possible add missing `.dta` extensions when performing path matching.

- Having the actual Stata working directory can help if the provided path is relative, but the filename has multiple matches in the supplement.

- Some commands generate files, like `save`. Here we may only want to map the directory, not the file itself, since the file may well not yet exist in the supplement.

- We may not have yet an exhaustive list of commands that use file paths as arguments. By default we assume that every argument in the `using` clause of a file path is a file path and store a list of other commands like `use` or `save` that use a file path directly as argument. We possible could systematically check for other commands that require files, by searching for "missing file" error codes when running the do files.

- It would probably be a bit more elegant to write the `find.path` function purely in Stata. But then who wants to learn so much Stata to write such a function? Also the R interface seems to work well so far.
