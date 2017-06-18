import codecs


writeout = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/merge_movies.txt","w" )
#actor = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actor_movies.txt")
#actress = open("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actress_movies.txt")
id = 0
print("start processing actor data")
# always break down due to a unicodeparseerror at 1920 lines in actor file, so have to throw a exception for this case
with open ('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actor_movies.txt',errors='ignore') as actor:
	#con = actor.read()
	#newactor = con.decode('gbk', 'ignore')
	for line in actor.readlines():
	# for each line, split by "\t\t", word is a list
		#line = line[:-1]
		word = line.split("\t\t")
		if len(word) < 11:
			continue
		writeout.write(str(id))
		id = id + 1
	# for word list, loop except the first item, which is actor/actress name
		for i in word[1:]:
		# only preserve movie name and year
			end = i.find(")")
			i = i[:end+1]
			i = i.strip()
			if str(i): 
				writeout.write("\t\t")
				writeout.write(str(i))
	# jump to next line
		writeout.write("\n")
print("Done!"+"\n"+"Only %d actors left in the dataset" % (id+1))
# processing actress
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
		writeout.write(str(id))
		id = id + 1
	# for word list, loop except the first item, which is actor/actress name
		for i in word[1:]:
		# only preserve movie name and year
			end = i.find(")")
			i = i[:end+1]
			i = i.strip()
			if str(i): 
				writeout.write("\t\t")
				writeout.write(str(i))
	# jump to next line
		writeout.write("\n")
# Don't forget to close the writeout stream
writeout.close()
print("Done!"+"\n"+"Only %d actors and actress left in the dataset" % (id+1))



### log 
# start processing actor data
# Done!
# Only 74599 actors left in the dataset
# start processing actress data
# Done!
# Only 113133 actors and actress left in the dataset


### format:
# {act_id}\t\t{movie1}\t\t{movie2}\t\t...{movie10}...
# at least 10 movies for each actor/actress