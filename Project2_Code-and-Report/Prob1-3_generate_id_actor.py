## use this py program to extract actor name by ID 

id_actor = dict()

with open('/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/id_actor.txt', encoding="latin1") as idactor:
    print("start processing id_movie dict")
    for line in idactor.readlines():
        word = line.split("\t\t")
        if word[0] not in id_actor:
            id_actor[word[0]] = word[1]

    mylist = [111843, 91669, 111843, 87535, 111843, 112573, 112685, 111843, 67880, 91669, 61554, 91669, 92309, 87535,
              112573, 34294, 112573, 19250, 70028, 91669]
    num = 1
    for item in mylist:
        print(str(num) + ". " + id_actor[str(item)])
        num += 1

    print("id_actor dict done!")


## program log:

# start processing id_movie dict
# 1. Williams, Sarah Anne

# 2. Katou, Emiri

# 3. Williams, Sarah Anne

# 4. Got, Yûko

# 5. Williams, Sarah Anne

# 6. Yoshida, Seiko

# 7. Yuki, Aoi

# 8. Williams, Sarah Anne

# 9. Tsukui, Kyôsei

# 10. Katou, Emiri

# 11. Shimizu, Kazuki

# 12. Katou, Emiri

# 13. Kingetsu, Mami

# 14. Got, Yûko

# 15. Yoshida, Seiko

# 16. Kenn

# 17. Yoshida, Seiko

# 18. Egawa, Daisuke

# 19. von Gomm, Peter

# 20. Katou, Emiri

# id_actor dict done!

# Process finished with exit code 0