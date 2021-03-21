#' CAT decision tree plot
#'
#' Generates a plot object to visualize the CAT decision tree
#'
#' @param object A \code{cat.dt} object
#' @param levels Number of levels to plot, starting from the first one.
#' @param tree Index of tree to plot. The total number of trees is given by \code{length(nodes$nodes[[1]])}.
#'
#' @return A ggplot2 object
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
#' 
#' @import ggplot2
#'
plot_tree = function(object, levels = 3, tree = 1){
  
  nodes = object
  
  # Check length
  if (levels > length(nodes$nodes)-1) {
    message(paste0("Using maximum number of levels: ", length(nodes$nodes)-1, 
                   "\nThis could take some time..."))
    levels = length(nodes$nodes)-1
  }
  
  if (levels < 2){
    message(paste0("Using minimum number of levels: ", 2))
    levels = 2
  }
  
  if (levels > 4){
    message("Displaying trees with more than four levels might result in a tree 
            with overlapping nodes and branches")
  }
  
  #Check tree index
  number_of_trees = length(nodes$nodes[[1]])
  
  if (tree > number_of_trees) {
    stop(paste0("Maximum value for tree is ", number_of_trees))
  }
  
  father = nodes$nodes[[1]][[tree]]$ID
  sons = nodes$nodes[[1]][[tree]]$ID_sons[,1]
  depen = rep(0, 6)
  
  for (i in sons) {depen = rbind(depen, c(father, i,0,0,0,0))}
  
  #Mark end of level
  depen = rbind(depen, rep(0, 6))
  
  new_fathers = sons
  
  if(levels > 2){
    for (i in 2:(levels-1)) {
      
      father = new_fathers
      new_fathers = c()
      
      for (j in 1:length(father)) {
        
        sons = nodes$nodes[[i]][[father[j]-(i*10000)]]$ID_sons[,1]
        new_fathers = c(new_fathers, sons)
      
        for (k in sons) {depen = rbind(depen, c(father[j], k,0,0,0,0))}
        
      }
      
      #Mark end and start of level
      depen = rbind(depen, rep(0, 6))
      
    }
  }
  lim = which(depen[, 1] == 0)
  
  #Calculate node positions for the last level to plot
  piec = depen[(lim[levels-1]+1):(lim[levels]-1), ] #Depen for this level
  lt = rep("solid", nrow(piec))
  
  aux = c() #Depen for this level and omitting repeated values in col. 2
  
  for (i in 1:nrow(piec)) {
    
    if (sum(aux[, 2] == piec[i, 2]) == 0) {
      aux = rbind(aux, c(piec[i, 1], piec[i, 2], 0, 0, 0, 0))
    }
    else {lt[i] = "dashed"}
    
  }
  
  sep = 0.2
  for(i in 1:nrow(aux)) { #X and Y coordinates of the plot node position
    
    aux[i, 5] = -1/2*sep*nrow(aux)+(i-1)/(nrow(aux)-1)*sep*nrow(aux)
    aux[i, 6] = -levels+1
    
  }
  
  fat = unique(aux[ ,1])
  for (i in fat) { #X and Y coordinates of the plot father node position
    
    rws = which(aux[, 1] == i)
    aux[rws, 3] = mean(aux[rws, 5])
    aux[rws, 4] = -levels+2
    
  }
  
  for (i in 1:nrow(aux)) {#Store the coordinates of the node
    
    ind = which(piec[, 2] == aux[i ,2]) 
    piec[ind, 5] = aux[i, 5]
    piec[ind, 6] = aux[i, 6]
    
  }
  
  for (i in 1:nrow(piec)) {
    ind = which(aux[, 1] == piec[i, 1])[1]
    if (is.na(ind)) {
      piec[i, 3] = mean(piec[which(piec[, 1] == piec[i, 1]), 5])}
    else{piec[i, 3] = aux[ind, 3]}
    piec[i, 4] = -levels+2

  }

  
  #Plot the last level. First the lines, then the points and text
  p = ggplot()
  for (i in 1:nrow(piec)) {
    
    nodf = piec[i, 1] -(levels - 1)*10000
    #nodes$nodes[[levels - 1]][[nodf]]$ID_sons
    
    nod = piec[i, 2] - (levels*10000)
    
    this_matrix <- nodes$nodes[[levels - 1]][[nodf]]$ID_sons
    this_response <- factor(this_matrix["Response"][this_matrix["ID_son"]==piec[i,2]], levels = 1:500)
    
    p = p +
      geom_line(aes_string(x = c(piec[i, 5], piec[i, 3]), 
                           y = c(piec[i, 6]+0.1, piec[i, 4]-0.1),
                           colour = this_response),
                linetype = lt[i])
    
  }
  
  for (i in 1:nrow(piec)) {
    
    nod = piec[i, 2] - (levels*10000)
    
    #If there is item (it is not the last)
    if (!is.na(nodes$nodes[[levels]][[nod]]$item)) {
      lab = as.character(nodes$nodes[[levels]][[nod]]$item)
    }else lab = 'SE'
      
    p = p +
      geom_point(aes_string(x = piec[i, 5], y = piec[i, 6]),
                 colour = "skyblue", size = 12)+
      geom_text(aes_string(x = piec[i, 5]), y = piec[i, 6], 
                           label = lab)
  }
  
  old_piec = piec
  if(levels > 2){
  #Plot the previous levels
  for (k in rev(2:(levels-1))) {
    
    piec = depen[(lim[k-1]+1):(lim[k]-1), ]
    lt = rep("solid", nrow(piec))
    
    aux = c()
    
    for (i in 1:nrow(piec)) {
      
      if (sum(aux[, 2] == piec[i, 2]) == 0) {
        aux = rbind(aux, c(piec[i, 1], piec[i, 2], 0, 0, 0, 0))
      }
      else{lt[i] = "dashed"}
      
    }
    
    for(i in 1:nrow(aux)) {
      
      equal = which(old_piec[, 1] == aux[i, 2])[1]
      
      if (is.na(equal)) {aux[i, 5] = -1/2*sep*nrow(aux)+
                                    (i-1)/(nrow(aux)-1)*sep*nrow(aux)}
      else  aux[i, 5] = old_piec[equal, 3]
      aux[i, 6] = -k+1
      
    }
    
    fat = unique(aux[ ,1])
    for (i in fat) {
      
      rws = which(aux[, 1] == i)
      aux[rws, 3] = mean(aux[rws, 5])
      aux[rws, 4] = -k+2
      
    }

    for (i in 1:nrow(aux)) {
      
      ind = which(piec[, 2] == aux[i ,2]) 
      piec[ind, 5] = aux[i, 5]
      piec[ind, 6] = aux[i, 6]
      
    }
    
    for (i in 1:nrow(piec)) {
      ind = which(aux[, 1] == piec[i, 1])[1]
      if (is.na(ind)) {
        piec[i, 3] = mean(piec[which(piec[, 1] == piec[i, 1]), 5])}
      else{piec[i, 3] = aux[ind, 3]}
      piec[i, 4] = -k+2
      
    }
    
    old_piec = piec
    
    #Check distances
    sep = sep*levels/k
    hap = 1
    while (hap) {
      hap = 0
      for (i in 2:nrow(piec)) {
        if((piec[i-1, 3]- piec[i, 3]) > 0 && abs(piec[i-1, 3]- piec[i, 3]) < sep) {
          hap = 1
          piec[i-1, 3] = piec[i-1, 3]-sep/2
          piec[i, 3] = piec[i, 3]+sep/2
        }
      }
    }
      
    for (i in 1:nrow(piec)) {
      
      nod = piec[i, 2] - (k*10000)
      
      nodf = piec[i, 1] - (k - 1)*10000
      this_matrix <- nodes$nodes[[k - 1]][[nodf]]$ID_sons
      this_response <- factor(this_matrix["Response"][this_matrix["ID_son"]==piec[i,2]], levels = 1:500)
      
      
      p = p +
        geom_line(aes_string(x = c(piec[i, 5], piec[i, 3]), 
                             y = c(piec[i, 6]+0.1, piec[i, 4]-0.1),
                             colour = this_response),
                  linetype = lt[i])
      
    }
    
    for (i in 1:nrow(piec)) {
      
      nod = piec[i, 2] - (k*10000)
      
      #If there is item (it is not the last)
      if (!is.na(nodes$nodes[[k]][[nod]]$item)) {
        lab = as.character(nodes$nodes[[k]][[nod]]$item)
      }else lab = 'SE'
      
      p = p +
        geom_point(aes_string(x = piec[i, 5], y = piec[i, 6]),
                   colour = "skyblue", size = 12)+
        geom_text(aes_string(x = piec[i, 5]), y = piec[i, 6], 
                             label = lab)
    }
    
  }
  }
  p = p +
    geom_point(aes_string(x = mean(piec[, 5]), y = 0),
               colour = "skyblue", size = 12)+
    geom_text(aes_string(x = mean(piec[, 5]), y = 0, 
                         label = as.character(nodes$nodes[[1]][[tree]]$item)))
    
  p + labs(y = "", x = "") + 
    theme_classic() + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())+
    labs(colour = "Response")
  
  
}
