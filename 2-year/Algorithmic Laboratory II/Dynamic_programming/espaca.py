"""

Implemente uma função que, dada uma frase cujos espaços foram retirados, 
tenta recuperar a dita frase. Para além da frase (sem espaços nem pontuação), 
a função recebe uma lista de palavras válidas a considerar na reconstrução 
da dita frase. Deverá devolver a maior frase que pode construir inserindo
espaços na string de entrada e usando apenas as palavras que foram indicadas 
como válidas. Por maior entende-se a que recupera o maior prefixo da string
de entrada. Só serão usados testes em que a maior frase é única.

"""

# 70%
# Temos de ver se tem palavras q estao contidas noutras

def init(palavras):
    res = {}
    for pal in palavras:
        res[pal] = set()
    return res

def removeSets(dic):
    r = {}
    for p in dic:
        if dic[p] != set():
            r[p] = dic[p]
    return r
    

def contencao(palavras):
    res = init(palavras)
    for main in palavras: #palavra a criar dicionario
        for pal in palavras:
            if pal != main:
                #Lógica importante
                mainLen = len(main)
                counter = 0
                for i in range(min(mainLen,len(pal))):
                    if pal[i] == main[i]:
                        counter += 1
                if counter == mainLen:
                    res[main].add(pal)
    return removeSets(res)


def hasBigger(frase,palavras): #returns smallest word that is bigger than current
    print("hasBigger received frase:",frase)
    if palavras != set():
        temp = []
        for c in frase:
            temp.append(c)
            x = "".join(temp)
            print("x:",x)
            if x in palavras:
                print("Left hasBigger | ", x, "is bigger than", x)
                return x
    print("Left hasBigger")
    return ""

def findWord(frase,palavras,contidas):
    temp = []
    res = ""
    for c in frase:
        temp.append(c)
        x = "".join(temp) # x is essentially just a string of temp
        print("x:",x)
        if x in palavras:
            inic = (len(res)-len(x))
            if inic < 0:
                inic = 0
            print("inic:",inic)
            fim = 0
            if (x in contidas and hasBigger(frase[inic:],contidas[x]) == ""):
                temp = []
                res += x + " "
            elif x not in contidas:
                temp = []
                res += x + " "
    return res

def espaca(frase,palavras):
    if palavras == []:
        return ""
    r = []
    temp = []
    contidas = contencao(palavras)
    print(contidas)
    res = findWord(frase,palavras,contidas)
    return res[:-1]