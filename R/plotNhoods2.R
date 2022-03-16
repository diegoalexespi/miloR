

### Plotting neighbourhood graph (custom) ###

#' Plot graph of neighbourhood
#'
#' Visualize graph of neighbourhoods
#'
#' @param x A \code{\linkS4class{Milo}} object
#' @param layout this can be (a) a character indicating the name of the \code{reducedDim} slot in the
#' \code{\linkS4class{Milo}} object to use for layout (default: 'UMAP') (b) an igraph layout object
#' @param colour_by this can be a data.frame of milo results or a character corresponding to a column in colData
#' @param subset.nhoods A logical, integer or character vector indicating a subset of nhoods to show in plot
#' (default: NULL, no subsetting)
#' @param size_range a numeric vector indicating the range of node sizes to use for plotting (to avoid overplotting
#' in the graph)
#' @param node_stroke a numeric indicating the desired thickness of the border around each node
#' @param ... arguments to pass to \code{ggraph}
#'
#' @return a \code{ggplot-class} object
#'
#' @author Emma Dann
#'
#' @examples
#' NULL
#'
#' @export
#' @rdname plotNhoodGraphLogFC
#' @import igraph
#' @import ggraph
#' @importFrom SummarizedExperiment colData<-
#' @importFrom RColorBrewer brewer.pal
plotNhoodGraphLogFC <- function(x, milo_res, alpha=0.05, res_column = "logFC",
                                layout="UMAP", colour_by=NA, subset.nhoods=NULL, size_range=c(0.5,3),
                                node_stroke= 0.3, show.edges, order.by = "size", plot.only.values = FALSE, 
                                bw.adjust = NULL, include.density = TRUE, ... ){
  
  if(!.valid_graph(nhoodGraph(x))){
    stop("Not a valid Milo object - neighbourhood graph is missing. Please run buildNhoodGraph() first.")
  }
  if (is.character(layout)) {
    if (!layout %in% names(reducedDims(x))) {
      stop(layout, "is not in reducedDim(x) - choose a different layout")
    }
  }
  
  ## Add milo results to colData
  signif_res <- milo_res
  signif_res[signif_res$SpatialFDR > alpha,res_column] <- 0
  colData(x)[res_column] <- NA
  colData(x)[unlist(nhoodIndex(x)[signif_res$Nhood]),res_column] <- signif_res[,res_column]
  
  
  ## Plot logFC
  colour_by <- res_column
  
  ## Check for valid nhoodGraph object
  if(!.valid_graph(nhoodGraph(x))){
    stop("Not a valid Milo object - neighbourhood graph is missing. Please run buildNhoodGraph() first.")
  }
  if (is.character(layout)) {
    if (!layout %in% names(reducedDims(x))) {
      stop(layout, "isn't in reducedDim(x) - choose a different layout")
    }
  }
  nh_graph <- nhoodGraph(x)
  
  ## Subset
  if (!is.null(subset.nhoods)) {
    nh_graph <- igraph::induced_subgraph(nh_graph, vids = which(as.numeric(V(nh_graph)$name) %in% unlist(nhoodIndex(x)[subset.nhoods])))
  }
  
  #get nh_graph coldata
  nh_graph_df <- data.frame(get.vertex.attribute(nh_graph))
  nh_id_ix <- as.integer(nh_graph_df$name)
  layout <- reducedDim(x, layout)[as.integer(nh_id_ix),]
  layout_names <- colnames(layout)
  nh_coldata <- cbind(colData(x)[nh_id_ix,], nh_graph_df, layout)
  nh_coldata <- as.data.frame(nh_coldata)
  if(order.by == "logFC"){
    nh_coldata <- nh_coldata[order(abs(nh_coldata$logFC)),]
    
  }
  
  pl <- ggplot(nh_coldata, aes(x = !!sym(layout_names[1]), y = !!sym(layout_names[2])))
  
  if(include.density){
    pl <- pl +  geom_density2d(color = "black", adjust = bw.adjust)
  }
  
  if(plot.only.values){
    pl <- pl + geom_point(data = nh_coldata[nh_coldata$logFC != 0,], aes(size = size, fill = logFC), shape = 21)
  } else {
    pl <- pl + geom_point(aes(size = size, fill = logFC), shape = 21)
  }
  
  pl <- pl  +
    scale_fill_gradient2()+
    theme_classic(base_size=14)

  pl
}


