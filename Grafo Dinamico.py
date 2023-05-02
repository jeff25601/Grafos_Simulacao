import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import networkx as nx

from networkx.drawing.nx_pydot import graphviz_layout

n = 11; taxa =  5; seeds = [1, 4, 9]; new_seeds = 3; profundidade = 3
def grafo(n: int, taxa: int, seeds: list, new_seeds = 3, profundidade = 3):
  
  depth = 0
  
  mat = np.empty((sum(len(seeds)*(3**np.arange(0, profundidade))*(n - np.arange(1, profundidade + 1))), 8), dtype = int)
  
  df_grafo = pd.DataFrame(mat, columns= ["time", "passado", "source", "target", "outw", "inw", "weight", "new_seeds"])
  
  depth_lengths =  [0] + (len(seeds)*(3**np.arange(0, profundidade))*(n - np.arange(1, profundidade + 1))).tolist()

  grafo_index = np.cumsum(depth_lengths)

  seed = np.repeat(seeds, np.repeat(np.array(n - (depth + 1)), [len(seeds)]).tolist())

  passado = np.repeat(['999'], [len(seed)])

  df_seeds = pd.DataFrame({'seed': seed, 'passado': passado})

  while(depth != profundidade):
    
    out_edge = np.random.poisson(lam = taxa, size = depth_lengths[depth+1])
    out_edge = np.where(out_edge==0, np.nan, out_edge)

    df1 = pd.DataFrame({'time': depth,
                     'passado': df_seeds["passado"].reset_index(drop = True),
                     'source': df_seeds["seed"].reset_index(drop = True),
                     'target': df_seeds.drop_duplicates().apply(lambda x: list(set(np.arange(1, n+1).tolist()) - set([int(i) for i in x.loc['passado'].split('-')] + [x.loc['seed']])), axis = 1).explode(ignore_index = True),
                     'outw': out_edge})
    
    df2 = pd.DataFrame({'passado': df_seeds["passado"].reset_index(drop = True),
                     'source': df_seeds["seed"].reset_index(drop = True),
                     'target': df_seeds.drop_duplicates().apply(lambda x: list(set(np.arange(1, n+1).tolist()) - set([int(i) for i in x.loc['passado'].split('-')] + [x.loc['seed']])), axis = 1).explode(ignore_index = True),
                     'inw': out_edge})
    
    df = df1.merge(df2, how = 'left', left_on= ['source', 'target', 'passado'], right_on=['target', 'source', 'passado'])

    df = df.drop(["source_y", "target_y"], axis = 1)

    df = df.rename(columns = {'source_x': 'source', 'target_x': 'target'})

    df["weight"] = df[["outw", "inw"]].mean(axis = 1, skipna = True)

    df["new_seeds"] = np.where(df.groupby(["source", "passado"])['weight'].rank(axis = 0, method = 'first', ascending = False, na_option = 'bottom') > 3, 0, 1)

    df["passado"] = df[["passado", "source"]].apply(lambda x: x['passado'] + '-' + str(x['source']), axis = 1)

    df_grafo.iloc[(grafo_index[depth]):grafo_index[depth+1]] = df

    seeds = df['target'][df['new_seeds'] == 1]

    passado = df['passado'][df['new_seeds'] == 1]
    
    depth = depth + 1

    seed = np.repeat(seeds, np.repeat(np.array(n - (depth + 1)), [len(seeds)]).tolist())

    passado = np.repeat(passado, np.repeat(np.array(n - (depth + 1)), [len(passado)]).tolist())
    
    df_seeds = pd.DataFrame({'seed': seed, 'passado': passado})
  
  df_grafo = df_grafo.dropna(subset = 'outw')

  df_grafo['passado'] = df_grafo['passado'].str.replace('999-', '')

  #df_grafo['source'] = df_grafo.apply(lambda x: str(x['time']) + '-' + str(x['source']), axis = 1) \\

  #df_grafo['target'] = df_grafo.apply(lambda x: str(x['time'] + 1) + '-' + str(x['target']), axis = 1) \\
  
  return(df_grafo)
  

teste = grafo(n = 11, taxa = 22, seeds = [1], new_seeds=3, profundidade=3)

# Inicio //

teste_wave0 = teste[teste['time'] == 0]

colors = np.where(teste_wave0['new_seeds'] == 0, "blue", "red")

teste_wave0 = teste_wave0.reset_index(drop = True)

G = nx.from_pandas_edgelist(teste_wave0, 'source', 'target')

pos = nx.nx_agraph.graphviz_layout(G, prog="twopi")
plt.figure(figsize=(8, 8))
nx.draw_networkx(G, pos, node_size=1000, alpha=0.5, 
                 edge_color = colors, 
                 with_labels=True)
plt.axis('equal')
plt.show()

# Primeira onda //

teste_wave1 = teste[teste['time'] == 1]

colors = np.where(teste_wave1['new_seeds'] == 1, "blue", "red")

teste_wave1 = teste_wave1.reset_index(drop = True)

G = nx.from_pandas_edgelist(teste_wave1, 'source', 'target')

pos = nx.nx_agraph.graphviz_layout(G, prog="twopi")
plt.figure(figsize=(8, 8))
nx.draw_networkx(G, pos, node_size=1000, alpha=0.5, 
                 edge_color = colors, 
                 with_labels=True)
plt.axis('equal')
plt.show()

# Segunda onda //

teste_wave2 = teste[teste['time'] == 2]

colors = np.where(teste_wave2['new_seeds'] == 1, "blue", "red")

teste = teste_wave2.reset_index(drop = True)

G = nx.from_pandas_edgelist(teste_wave2, 'source', 'target')

pos = graphviz_layout(G, prog="twopi")
plt.figure(figsize=(8, 8))
nx.draw_networkx(G, pos, node_size=20, alpha=0.5, 
                 edge_color = colors, 
                 with_labels=False)
plt.show()