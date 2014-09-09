#' Run Phylocom's bladj from R
#' 
#' @import ape
#' @importFrom phytools read_newick
#' @param tree (phylo/character) If left NULL, it is expected that you already have a 
#' phylo file with your newick tree in your directory with your phylocom executable
#' @param ages (data.frame) If left NULL, it is expected that you already have an 
#' ages file with your node names and ages in your directory with your Phylocom executable
#' @param path (character) Path to the folder with at least the Phylocom executable
#' @param fixroot (logical) If TRUE root name is changed to the oldest root in your ages file.
#' If FALSE (default), 
#' @export
#' @examples \donttest{
#' taxa <- names_list("species", 15)
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' # set path to where your dir is with phylocom executable, and optionally phylo 
#' # and/or ages files
#' path = "path to phylocom executable directory" 
#' g <- rbladj(tree, path=path, fixroot=TRUE)
#' g$edge.length
#' plot(g)
#' }
rbladj <- function(tree=NULL, ages=NULL, path=NULL, fixroot=FALSE)
{
  #download phylocom
  download.file("http://phylodiversity.net/phylocom/phylocom-4.2.zip")
  
  #set path
  if(!is.null(path)){
    setwd(path)
  } else
  { stop("You must provide a path to where phylocom executable is stored on your machine") }
  
  #phylo
  if(!is.null(tree)){
    if(any(class(tree) == "character")){ tree <- read_newick(file=paste(path,"phylo",sep="")) } else
      if(any(class(tree) == "phylo")) { tree <- tree } else {
        stop("need newick tree as text string or phylo object")
      }
    setwd(path)
    write.tree(tree, "phylo")
  }
  
  #ages
  if(is.null(ages)){
    tmp <- tryCatch(read.table(paste(path, "ages", sep="")), error = function(e) e)
    if(any(class(tmp) == "error")){
      stop("You must provide an ages data.frame or a path to the ages file")
    }
  } else
  {
    if(is(ages, "data.frame")){
      write.table(ages, file = paste(path,"ages",sep=""), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)      
    } else {
      stop("ages, it not a path, must be a data.frame with two columns, node name, and node age")
    }
  }
  
  # check if root node in phylogeny is in the ages file
  if(is.null(tree)){ tree <- read_newick(file=paste(path,"phylo",sep="")) }
  if(is.null(ages)){ 
    ages <- read.table(paste(path, "ages", sep="")) 
    names(ages) <- c("names","ages")
  }
  rootnode <- as.character(tree$node.label[1])
  if(!rootnode %in% as.character(ages[,1])){
    if(fixroot){
      ages_sorted <- sort_df(ages, "ages")
      newroot <- as.character(ages_sorted[nrow(ages_sorted),1])
      tree <- read.tree(text=sub(rootnode, newroot, write.tree(tree)))
      setwd(path)
      write.tree(tree, "phylo")
    } else
    {
      stop("Your root node in your phylogeny must be in your ages file - names must match exactly")
    }
  }
  
  #bladj
  if(.Platform$OS.type != "unix"){
    setwd(path)
    shell("phylocom bladj > phyloout.txt")
  } else
  {
    setwd(path)
    system("./phylocom bladj > phyloout.txt")    
  }
  
  #read tree back in
  tr <- read_newick(paste(path, "phyloout.txt", sep=""))
  return( tr )
}