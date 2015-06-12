#'Attaches the query genome id to the dataframe (if data.table provided, will convert to data.frame).
#'
#'@param \code{bh.table} bh data table that needs to have first two colums converted to rownames
#'@param \code{sep} separator symbol, default "X"
#'@return data.frame with proper names attached
#'@examples
#'bh.folder<-"gBLASTer/bh"
#'test.table.sign<-sign.bh.table(bh.table = read.bhs(bh.folder))
#'
sign.bh.table<-function(bh.table, sep="X"){
  bh.table<-as.data.frame(bh.table)
  new.row.names<-apply(bh.table,1, function(i){
    return(paste(i[2],as.numeric(i[1]),sep = sep))
  })
  row.names(bh.table)<-new.row.names
  return(bh.table[,-c(1,2)])
}