#'Use this function to send a tree to the iTOL API, receive the output and save as a pdf file.
#'@param \code{newick.file} a newick-formatted file
#'@param \code{project.name} a mandatory for the iTOL service project name
#'@param \code{output.file} a file to save the output pdf
#'@retrn output file name for the generated pdf
#'@examples
plot.itol<- function(newick.file, project.name="Default", output.file){
  #request
  req<-POST(url = "http://itol.embl.de/batch_uploader.cgi",
            encode = "multipart", body=list(
              treeFile=upload_file(newick.file),#the tree itself in newick format
              treeFormat="newick", #this says that the tree is in newick and is best to indicate explicitly
              projectName="Default" #just a project name, mandatory though
            ))
  id<- str_replace_all(string = str_trim(toString(req)),pattern = "SUCCESS: ", replacement = "")#this is simply to extract the ID from the server responce
  print(id)
  #download
  pl<-POST(url = "http://itol.embl.de/batch_downloader.cgi",
           encode = "multipart", body=list(
             tree= id,
             format="pdf",#we want it in pdf
             displayMode="unrooted",#we want it in circular and unrooted
             resolution=300,#print resolution, makes no sence for a vector pdf file, but there might ne smth like fonts for internal nodes, that get rasterized
             fontSize=48,#this one is the highest number of the font size, otherwise the letters start to overlap
             lineWidth=1,#this is the line stoke, enough to reflect nicely
             hideRanges=0,#we do not have any ranges here, so it is good idea to switch this off
             scaleFactor=0.8#this is the minimum scaling factor, if made smaller, the words begin to overlap (scaling here is the width of the plot if looked at horyzontally)
           ))
  #extract the binary object from the responce and save it as pdf (which it is).
  writeBin(pl$content, con = output.file)
  return(output.file)
}