
sings = [
    '-',
    "'",
    ',',
    '*',
    '.',
    '"',

]

f = open('temp1.txt', 'r', encoding='UTF-8')
t = open('new_recept.txt', 'w', encoding='UTF-8')
a = []
for i in f:
    temp = i.split()
    for sing1 in sings:
        temp = temp.replace(sign1,' ')
    t.write('_'.join(temp) + "\n")
    