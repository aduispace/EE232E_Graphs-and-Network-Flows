## QUESTION 4
## This script is for calculating jaccard-index between movies with greater than 5 actors

## Return a list of movies with greater than 5 actors
with open('/Users/huxiongfeng/Desktop/ee232eWorkSpace/Project2/project_2_data/movie_actor/movie_genre.txt', 'r', errors = 'ignore') as f1:
    movie_lines = f1.readlines()
movie_lines = [line.rstrip('\n') for line in movie_lines]# read the file line by line

movie_frequency = dict()# create an empty dict for movie and its number of actors

for row in movie_lines:
  row = row.split("\t\t")
  movie_frequency[row[0]] = 0

with open('/Users/huxiongfeng/Desktop/ee232eWorkSpace/Project2/project_2_data/merge_movies.txt', 'r', errors = 'ignore') as f2:
    actor_movies_lines = f2.readlines()
actor_movies_lines = [line.rstrip('\n') for line in actor_movies_lines]# read the file line by line

for row in actor_movies_lines:
    row = row.split("\t\t")
    for i in range(1, len(row)):
        if row[i] in movie_frequency:
            movie_frequency[row[i]] += 1

movie_list = [] ## create an empty list for movies with greater than 5 actors
for k,v in movie_frequency.items():
    if v > 5:
        movie_list.append(k) 

##  Return a dict for movie(with greater than 5 actors) and its actors
movie_actors = dict()# create an empty dict for mapping movie to a list of its actor ids
for movie in movie_list:
    movie_actors[movie] = []

for row in actor_movies_lines:
    row = row.split("\t\t")
    for i in range(1, len(row)):
        if row[i] in movie_list:
            movie_actors[row[i]].append(row[0])
            print(movie_actors[row[i]])

actor_movies = dict() # map actor to its list of movies       
for row in actor_movies_lines:
    row = row.split("\t\t")
    actor_movies[row[0]] = []
    for i in range(1, len(row)):
        actor_movies[row[0]].append(row[i])
            
   
## Calculate jaccard-index between movies
output = []
for movie_i, actors in sorted(movie_actors.items()):# loop over all movies with more than 5 actors(movie index = i)
    for actor in actors:# loop over all actors in movie i(actor index = j)
        for movie in actor_movies[actor]:# loop over movies that actor j has played in
            if  movie in movie_actors and movie != movie_i:
                n = len(set(movie_actors[movie_i]).intersection(set(movie_actors[movie])))
                if n == 0:
                    continue
                if float(len(movie_actors[movie_i]) + len(movie_actors[movie]) - n) == 0:
                    continue
               JI = (n / float(len(movie_actors[movie_i]) + len(movie_actors[movie]) - n))
               output.append(str(movie_i) + "\t\t" + str(movie) + "\t\t" + str(JI))
               print(len(output))
##
file = open('jaccard_edge_weight.txt', 'w')

for item in output:
  file.write("%s\n" % item)
  
