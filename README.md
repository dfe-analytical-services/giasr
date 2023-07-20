# 1. Introduction 
This package is designed to simplify downloading and basic data wrangling of published DfE data, primarily data from [https://get-information-schools.service.gov.uk/Downloads](https://get-information-schools.service.gov.uk/Downloads)

# 2. Getting Started
Install the package from GitHub using {remotes}.

```
install.packages("remotes")  # if not already installed
remotes::install_github
```

The package depends on {dplyr}, {janitor}, {readr}, {readxl}, {rlang}, {rvest}, {sf}, {tibble}, {utils}, which are also installed with {giasr}.

# 3. Features

{giasr} has functions for downloading and processing data from several [Get Information About Schools (GIAS)](https://get-information-schools.service.gov.uk/Downloads) datasets. However, it is your responsibility to QA the outputs and ensure that the results you get are reasonable. 

This is particularly the case for functions relating to URN linking over time and School-Trust links. For these functions heuristics and exceptions have been implemented in the code to deal with issues and inconsistencies in the data.

# 4. Contribute
There is a lot of potential to build on the functions to include items such as post-code linking to specific ONS geographies. If you would like to contribute please add an [issue]("https://github.com/dfe-analytical-services/giasr/issues") or a [pull request]("https://github.com/dfe-analytical-services/giasr/pulls").

* For major changes or additions, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed.

Pull request process
* Fork the package and clone onto your computer.
* Install all development dependencies with devtools::install_dev_deps(), and then make sure the package passes R CMD check by running devtools::check(). If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
* Create a Git branch for your pull request (PR). The new branch name should start with your initials, e.g. 'xy-fix-bug'.
* Make your changes, commit to git, and then create a PR. The title of your PR should briefly describe the change. The body of your PR should contain Fixes #issue-number.
