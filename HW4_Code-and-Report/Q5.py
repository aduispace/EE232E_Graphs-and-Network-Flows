
from collections import defaultdict
import sys
import csv

tsp = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/HW4/tsp.txt", "w")


sys.setrecursionlimit(1500)
graph = list()
with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/HW4/double_mst.csv') as mst:
    print("start processing mst to graph")
    i = 0
    for row in mst:
        if i != 0:
            a = row.split(',')[0]
            b = row.split(',')[1]
            a = a.strip('"')
            b = b.strip('"')
            temp = (a, b)
            graph.append(temp)
        i += 1

# print (graph)



def find_euler_tour(graph):
    tour = []
    E = graph

    numEdges = defaultdict(int)

    def find_tour(u):
        for e in E:
            if u == e[0]:
                u,v = e
                E.remove(e)
                find_tour(v)
            elif u == e[1]:
                v,u = e
                E.remove(e)
                find_tour(v)
        tour.insert(0,u)

    for i,j in graph:
        numEdges[i] += 1
        numEdges[j] += 1

    start = graph[0][0]
    for i,j in numEdges.items():
        if j % 2 > 0:
            start = i
            break

    current = start
    find_tour(current)

    if tour[0] != tour[-1]:
        return None
    return tour

tour = find_euler_tour(graph)

for i in tour:
    tsp.write(i + "\n")
print("All Done")
tsp.close()