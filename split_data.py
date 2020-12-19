# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 23:58:03 2020

@author: tneha
"""



with open('sample2.txt', 'r', encoding="utf8") as myfile:
  data = myfile.read()
  


  
d = data.split("____________________________________________________________") 

for i in range(500):
    filename = "maddie/data/" + str(i) + ".txt"
    with open(filename, "w", encoding="utf8") as output:
        output.write(d[i])
        
#files 0 and 499 are irrelevant (because of the way the input string is formatted) but I left them in the data folder to give you context      