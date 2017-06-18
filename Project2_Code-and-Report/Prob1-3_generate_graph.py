# In this script, we will generate three dicts:
# (1) id_name->{id:name} (finally abandon this thought, R cannot support this method to read graph)(2) id_movie->{id:movie1,2,3...} (3)movie_id->{movie:id1,id2,id3}

edgefile = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/edge_weight.txt", "w")

# part one: construct three dicts

# id_name = dict()
id_movie = dict()
movie_id = dict()

# print("start processing id_name dict")
# with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/id_actors.txt') as idactors:
#     for line in idactors.readlines():
#         word = line.split("\t\t")
#         # don't need to check duplicate items
#         id_name[word[0]] = word[1]
# print("id_name dict done!")

with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/merge_movies.txt') as idmovie:
    print("start processing id_movie dict")
    for line in idmovie.readlines():
        word = line.split("\t\t")
        for movie in word[1:]:
            # word[0] is id, note in id_movie, movie is value
            if word[0] not in id_movie:
                id_movie[word[0]] = list()
                id_movie[word[0]].append(movie)
            else:
                id_movie[word[0]].append(movie)
    print("id_movie dict done!")

with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/merge_movies.txt') as movieid:
    print("start processing movie_id dict")
    for line in movieid.readlines():
        word = line.split("\t\t")
        for movie in word[1:]:
            # word[0] is id, note in movie_id, id is value
            if movie not in movie_id:
                movie_id[movie] = list()
                movie_id[movie].append(word[0])
            else:
                movie_id[movie].append(word[0])
    print("movie_id dict done!")

# for i in movie_id:
#     if len(movie_id[i]) == 0:
#         print(i)
# exit()

print("start processing edge_weight")

# part two: construct edge_weight file
edge_number = 0
# i is the actor, j is i's movies, k is other actors in j
# project2 spec not excluded self pointing
for i in id_movie:
    i_weight = dict() # i_weight: {k: [temp, i_k_weight]}
    iweight = len(id_movie[i])
    for j in id_movie[i]:
            for k in movie_id[j]:
                if k not in i_weight:
                    edge_number += 1
                    i_weight[k] = list()
                    i_weight[k].append(1)
                    weight = float(1)/int(iweight)
                    i_weight[k].append(weight)
                else:
                    i_weight[k][0] += 1
                    weight = float(i_weight[k][1])/int(iweight)
                    i_weight[k][1] = weight
    for item in i_weight:
        weight = str(i_weight[item][1])
        weight = weight.strip()
        if str(i) and str(item) and weight:
            edgefile.write(str(i)+"\t\t"+str(item)+"\t\t"+weight+"\n")# jump to another line
        else:
            continue
        # edgefile.write("\n")
edgefile.close()
print("edge_weight dict done!" + "Producing %d weighted edges" % (edge_number))


print("all done!")

### log:
# start processing id_movie dict
# id_movie dict done!
# start processing movie_id dict
# movie_id dict done!
# start processing edge_weight
# edge_weight dict done!Producing 34267114 weighted edges
# all done!

### data format:
# {id1}\t\t{id2}\t\t{weight}
