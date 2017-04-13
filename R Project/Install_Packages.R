#BASIC PACKAGES
install.packages(c("rstudioapi", "glmnet", "randomForest"), dependencies=TRUE)

#FOR TREE VISUALIZATION
#POGLEDAJ - http://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree
# options(repos='http://cran.rstudio.org')
# have.packages <- installed.packages()
# cran.packages <- c('devtools','plotrix','randomForest','tree')
# to.install <- setdiff(cran.packages, have.packages[,1])
# if(length(to.install)>0) install.packages(to.install)
# 
# library(devtools)
# if(!('reprtree' %in% installed.packages())){
#   install_github('araastat/reprtree')
# }
# for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))