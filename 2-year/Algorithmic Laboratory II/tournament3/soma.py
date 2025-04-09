"""

Implemente uma função que recebe uma lista de números inteiros e retorna a soma máxima
que é possível obter duplicando números da lista, mas sem duplicar números adjacentes.

"""
memo = {}

def calc(nums,soma,flag):
    string = str((nums,soma))
    r = soma
    if string in memo:
        r = memo[string]
    else:    
        if nums == []:
            r = soma
        elif len(nums) == 1:
            if flag:
                soma += 2*nums[0]
                r = soma
            else:
                soma += nums[0]
                r = soma
        else:
            if flag:
                u1soma = soma + nums[0]*2
                u1 = calc(nums[1:],u1soma,False)
                u2soma = soma + nums[0]
                u2 = calc(nums[1:],u2soma,True)
                if u1 > u2:
                    r = u1
                else:
                    r = u2
            else:
                r = calc(nums[1:],soma + nums[0],True)
    memo[string] = r
    return r

def soma(nums):
    global memo
    memo = {}
    if nums == []:
        return 0
    return calc(nums,0,True)