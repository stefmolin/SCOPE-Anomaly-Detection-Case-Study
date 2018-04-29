library(devtools)

pkg_name <- 'modelAnalyzeR'

# first time build
if(!file.exists(pkg_name)){
  create(pkg_name)
}

# build package updating documentation beforehand
document(pkg_name)
install.packages(build(pkg_name), repos = NULL)
