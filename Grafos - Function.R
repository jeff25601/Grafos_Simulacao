library(tidyverse)
rm(list = ls())


# Simulate values in the edges
n = 11; taxa =  5; seeds = c(1, 4, 9); new_seeds = 3; profundidade = 4

grafo <- function(n, taxa, seeds, new_seeds = 3, profundidade){
  
  depth = 0
  
  grafo = matrix(data = NA, nrow = sum(length(seeds)*(3^(0:(profundidade-1)))*(n - 1:profundidade)), ncol = 8)
  
  grafo = as.data.frame(grafo)
  
  colnames(grafo) = c("time", "passado", "source", "target", "outw", "inw", "weight", "new_seeds")
  
  depth_lengths = c(0 , length(seeds)*(3^(0:(profundidade-1)))*(n - 1:profundidade))
  grafo_index = cumsum(depth_lengths)
  
  df_seeds = data.frame(seed = rep(seeds, each = n-(depth+1)), 
                        passado = list(999))
  colnames(df_seeds) = c("seed", "passado")
  
  while(depth != profundidade){
    
    out.edge <- rpois(depth_lengths[depth+2], taxa)
    out.edge[out.edge == 0] = NA
    
    df <- data.frame(time = depth,
                     source = df_seeds$seed,
                     target = df_seeds %>% 
                       unique() %>% 
                       apply(MARGIN = 1, FUN = function(x) return((1:n)[!(1:n) %in% c(unlist(x[2]), x[1])])) %>%
                       c,
                     outw = out.edge) %>% 
      mutate(passado = df_seeds$passado) %>%
      select(time, passado, source, target, outw)
    
    df <- df %>% left_join(df %>% 
                             rename(inw = outw) %>% 
                             select(-time), 
                           by = c("source" = "target", "target" = "source", "passado" = "passado"))
    
    
    
    df <- df %>%
      rowwise() %>% 
      mutate(weight=mean(c(inw,outw), na.rm = T)) %>%
      group_by(source, passado) %>%
      arrange(desc(weight), .by_group = T)
    
    
    df <- df %>%
      group_by(source, passado) %>%
      mutate(n = 1:n(),
             new_seeds = case_when(
               n %in% 1:new_seeds ~ 1,
               TRUE ~ 0
             )) %>%
      select(-n) %>%
      ungroup() %>% 
      rowwise() %>% 
      mutate(passado = list(list(passado, source)))
    
    grafo[(grafo_index[depth+1] + 1):grafo_index[depth+2], 1:8] = df
    
    seeds <- df %>% filter(new_seeds == 1) %>% select(target) %>% pull()
    
    passado = df %>% filter(new_seeds == 1) %>% select(passado) %>% pull()
    
    depth = depth + 1
    
    df_seeds = data.frame(seed = rep(seeds, each = n-(depth+1)))
    df_seeds = df_seeds %>% mutate(passado = rep(passado, each = n-(depth+1)))
    
  }
  
  grafo = grafo %>% filter(!is.na(outw)) %>% rowwise() %>% mutate(passado = list(unique(unlist(passado))[unique(unlist(passado)) != 999]))
  
  return(grafo)
  
}


  
teste <- grafo(11, 5, seeds = c(1, 4, 9), new_seeds = 3, profundidade = 4)

pathString = str_remove(as.character(teste$passado), pattern = "c\\(") %>%
  str_remove(pattern = "\\)") %>% 
  str_replace_all(pattern = ", ", "/")

pathString = paste("Begin", pathString, sep = "/")

teste$pathString = pathString

teste_grafo = as.Node(teste)

SetGraphStyle(teste_grafo, rankdir = "TB")


plot(teste_grafo)

library(networkD3)

grafo_list <- ToListExplicit(teste_grafo, unname = T)
radialNetwork(grafo_list)

teste_grafo

