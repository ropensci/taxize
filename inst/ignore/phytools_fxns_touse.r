#' read newick files, copied directly from phytools
#' @param file file path
#' @param text text string
#' @export
#' @keywords internal
read_newick <- function (file = "", text) 
{
    if (file != "") 
        text <- scan(file, sep = "\n", what = "character")
    if (length(text) > 1) {
        tree <- lapply(text, newick)
        class(tree) <- "multiPhylo"
    }
    else tree <- newick(text)
    return(tree)
}


#' Newick function required by read_newick
#' @param text input text
#' @export
#' @keywords internal
newick <- function (text) 
{
    text <- unlist(strsplit(text, NULL))
    tip.label <- vector(mode = "character")
    node.label <- vector(mode = "character")
    edge <- matrix(c(1, NA), 1, 2)
    edge.length <- vector()
    currnode <- 1
    Nnode <- currnode
    i <- j <- k <- 1
    while (text[i] != ";") {
        if (text[i] == "(") {
            if (j > nrow(edge)) 
                edge <- rbind(edge, c(NA, NA))
            edge[j, 1] <- currnode
            i <- i + 1
            if (is.na(match(text[i], c("(", ")", ",", ":", ";")))) {
                temp <- get_label(text, i)
                tip.label[k] <- temp$label
                i <- temp$end
                edge[j, 2] <- -k
                k <- k + 1
                if (text[i] == ":") {
                  temp <- getEdgeLength(text, i)
                  edge.length[j] <- temp$edge.length
                  i <- temp$end
                }
            }
            else if (text[i] == "(") {
                Nnode <- Nnode + 1
                currnode <- Nnode
                edge[j, 2] <- currnode
            }
            j <- j + 1
        }
        else if (text[i] == ")") {
            i <- i + 1
            if (is.na(match(text[i], c("(", ")", ",", ":", ";")))) {
                temp <- get_label(text, i)
                node.label[currnode] <- temp$label
                i <- temp$end
            }
            if (text[i] == ":") {
                temp <- getEdgeLength(text, i)
                if (currnode > 1) {
                  ii <- match(currnode, edge[, 2])
                  edge.length[ii] <- temp$edge.length
                }
                else root.edge <- temp$edge.length
                i <- temp$end
            }
            if (currnode > 1) 
                currnode <- edge[match(currnode, edge[, 2]), 
                  1]
        }
        else if (text[i] == ",") {
            if (j > nrow(edge)) 
                edge <- rbind(edge, c(NA, NA))
            edge[j, 1] <- currnode
            i <- i + 1
            if (is.na(match(text[i], c("(", ")", ",", ":", ";")))) {
                temp <- get_label(text, i)
                tip.label[k] <- temp$label
                i <- temp$end
                edge[j, 2] <- -k
                k <- k + 1
                if (text[i] == ":") {
                  temp <- getEdgeLength(text, i)
                  edge.length[j] <- temp$edge.length
                  i <- temp$end
                }
            }
            else if (text[i] == "(") {
                Nnode <- Nnode + 1
                currnode <- Nnode
                edge[j, 2] <- currnode
            }
            j <- j + 1
        }
    }
    Ntip <- k - 1
    edge[edge > 0] <- edge[edge > 0] + Ntip
    edge[edge < 0] <- -edge[edge < 0]
    edge.length[is.na(edge.length)] <- 0
    if (length(edge.length) == 0) 
        edge.length <- NULL
    node.label[is.na(node.label)] <- ""
    if (length(node.label) == 0) 
        node.label <- NULL
    tree <- list(edge = edge, Nnode = as.integer(Nnode), tip.label = tip.label, 
        edge.length = edge.length, node.label = node.label)
    class(tree) <- "phylo"
    return(tree)
}

#' get label function from phytools
#' @param text input text
#' @param start start at
#' @export
#' @keywords internal
get_label <- function (text, start) 
{
    i <- 0
    label <- vector()
    while (is.na(match(text[i + start], c(",", ":", ")")))) {
        label[i + 1] <- text[i + start]
        i <- i + 1
    }
    return(list(label = paste(label, collapse = ""), end = i + 
        start))
}