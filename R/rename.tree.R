#'Use this function to rename a tree of type \code{phylo} with the names from the given legend. If a genome id is not marked with separator marker (default is "X"), then it will be marked as "OUTSIDER"
#'@param \code{tree} is a tree of type \code{phylo}, note, only those labels that exist in the legend as genome codes will be replaced
#'@param \code{legend} regular legend, with "id_genomes" and "name" fields marked in the column names
#'@param \code{sep} separator symbol, defalut is "X"
#'@return a tree of type \code{phylo} renamed from genom short codes to full names
#'@examples
rename.tree<- function(tree, legend, sep="X"){
  tree$tip.label<-sapply(tree$tip.label,function(i){
    if(grepl(x=i, pattern = sep,fixed = TRUE)){
      return(paste0(legend$name[which(legend$id_genomes==extract_numeric(i))],"_genome"))
    }else{
      return(paste0(legend$name[which(legend$id_genomes==extract_numeric(i))],"_OUTSIDER>>>>>>"))
    }
  })
  return(tree)
}