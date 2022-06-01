# devtools requires
# FROM APT
#   apt install libssl-dev libxml2-dev pandoc
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

print("installing rmarkdown")
install.packages("rmarkdown")
print("installing knitr")
install.packages("knitr")
print("installing rafalib")
install.packages("rafalib")
print("installing BiocManager")
install.packages("BiocManager")
print("installing devtools")
install.packages("devtools")

print("installing genefilter")
BiocManager::install("genefilter") 
# Biobase is a dependency of genefilter, so it should install automatically
# BiocManager::install("Biobase")

print("installing GSE5859Subset")
devtools::install_github("genomicsclass/GSE5859Subset")
print("installing GSE5859")
devtools::install_github("genomicsclass/GSE5859")