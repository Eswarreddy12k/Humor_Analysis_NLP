import re
import csv
import os
for filename in os.listdir('C:/Users/eswar/Downloads/FG_dataset/'):
    root, ext = os.path.splitext(filename)
    f_path='C:/Users/eswar/Downloads/FG_dataset/'+filename
    season_episode=filename.split()
    season,episode=season_episode[3].split("x")
    with open(f_path, 'r',errors="ignore") as h:
        sub = h.readlines()
    fields = ['Season', 'Episode', 'Dialouge']
 
    # name of csv file
    filename = "fg_dataset1_3.csv"
 
    # writing to csv file



    x=['0','1','2','3','4','5','6','7','8','9','<']
    l=[]
    for i in sub:
        if(len(i)>0 and i[0] not in x):
            if(i!="\n"):
                if(i[0]=="-"):
                    l.append(i[1:len(i)])
                else:
                    l.append(i[:len(i)])
    rows=[]
    for i in l:
        if(len(i)>0):
            rows.append([season,episode,i])

    
    with open(filename, 'a') as csvfile:
    # creating a csv writer object
        csvwriter = csv.writer(csvfile)
        # writing the data rows
        csvwriter.writerows(rows)