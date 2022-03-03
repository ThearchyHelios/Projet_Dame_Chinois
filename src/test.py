'''
Author: JIANG Yilun
Date: 2022-03-03 10:10:48
LastEditTime: 2022-03-03 10:15:26
LastEditors: JIANG Yilun
Description: 
FilePath: /Projet_Dame_Chinois/src/test.py
'''
from random import randint

x = int(input("x = "))
y = int(input("y = "))
z = int(input("z = "))

if x + y + z != 0:
    print("a + b + c != 0")
else:
    if (x <= 3 and x >= -3) and (y <= 3 and y >= -3):
        print("True")
    else:
      if (y <= 3 and y >= -3) and (z <= 3 and z >= -3):
          print("True")
      else:
        if (x <= 3 and x >= -3) and (z <= 3 and z >= -3):
            print("True")
        else:
          print("False")