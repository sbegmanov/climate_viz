library(renv)

# use ?renv for more info. Shortly, the project works with 
# the packages' versions of that time.

# install environment packages for the project
renv::init()
  
# sample of installing needed package
renv::install("mgcv")

# for package update check within the project
renv::snapshot()

# check package directory
.libPaths()

# check for package existence
find.package()

# after installation a new package for the project, use it within the project
# and then snapchot() for renv.
