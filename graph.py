import networkx as nx
import matplotlib.pyplot as plt

f = open("file.txt", "r")

pherm_mat = f.readline()
cities_coords = f.readline()

G=nx.Graph()

cities_coords = eval(cities_coords.split()[0])

for i, coord in enumerate(cities_coords):
    (x, y) = coord
    G.add_node(i,pos=(x,y))

pherm_mat = eval(pherm_mat.split()[0])
for i, p in enumerate(pherm_mat):
    for j, val in enumerate(p):
        if val != 1:
            G.add_edge(i,j,weight=val)

pos=nx.get_node_attributes(G,'pos')
nx.draw(G,pos)
labels = nx.get_edge_attributes(G,'weight')
nx.draw_networkx_edge_labels(G,pos,edge_labels=labels)
plt.savefig("graph")

# G=nx.Graph()
# i=1
# G.add_node(i,pos=(i,i))
# G.add_node(2,pos=(2,2))
# G.add_node(3,pos=(1,0))
# G.add_edge(1,2,weight=0.5)
# G.add_edge(1,3,weight=9.8)
# pos=nx.get_node_attributes(G,'pos')
# nx.draw(G,pos)
# labels = nx.get_edge_attributes(G,'weight')
# nx.draw_networkx_edge_labels(G,pos,edge_labels=labels)
# plt.savefig("test")
