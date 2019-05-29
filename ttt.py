from turtle import*
import math
side = 50
forward(side)
right(90)
forward(side*3)
left(90)
forward(side)
left(90)
forward(side*3)
right(90)
forward(side)
right(90)
forward(side)
right(90)
forward(side*3)
left(90)
forward(side)
left(90)
forward(side*3)
right(90)
forward(side)
right(90)
j = 0
while j < 4:
    forward(side*3)
    right(90)
    j+=1
right(135)
forward(math.sqrt(side*3))
left(135)
j = 0
forward(side)
while j<3:
    left(135)
    forward(sqrt(side))
    right(135)
    forward(side*2)
exitonclick()


