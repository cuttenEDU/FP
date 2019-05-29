from turtle import*
i = 0
j = 0
line = 200
a = 10
while i <108:
    j = 0
    while j<4:
        forward(line)
        left(90)
        j+=1
    line= line * 0.97
    left(a)
exitonclick()