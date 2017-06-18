# for the reference of id <-> name
import codecs

idfile = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actor_id.txt", "w")

id = 0
print("start processing actor data")

with open ('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actor_movies.txt',errors='ignore') as actor:
	#con = actor.read()
	#newactor = con.decode('gbk', 'ignore')
	for line in actor.readlines():
	# for each line, split by "\t\t", word is a list
		#line = line[:-1]
		word = line.split("\t\t")
		if len(word) < 11:
			continue
		word[0].strip(" ")
		word[0].strip("\t")
		idfile.write(str(word[0])+"\t\t"+str(id))
		id = id + 1
		idfile.write("\n")
print("Done!"+"\n"+"Only %d actors left in the dataset" % (id+1))

print("start processing actress data")
with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actress_movies.txt',errors='ignore') as actress:
	#con = actress.read()
	#newactress = con.decode('gbk', 'ignore')
	for line in actress.readlines():
	# for each line, split by "\t\t", word is a list
		#line = line[:-1]
		word = line.split("\t\t")
		if len(word) < 11:
			continue
		word[0].strip(" ")
		word[0].strip("\t")
		idfile.write(str(word[0])+"\t\t"+str(id))
		id = id + 1
		idfile.write("\n")
# Don't forget to close the writeout stream
idfile.close()
print("Done!"+"\n"+"Only %d actors and actress left in the dataset" % (id+1))

###  log 
# start processing actor data
# Done!
# Only 74599 actors left in the dataset
# start processing actress data
# Done!
# Only 113133 actors and actress left in the dataset

### format:
# {id}/t/t{name}