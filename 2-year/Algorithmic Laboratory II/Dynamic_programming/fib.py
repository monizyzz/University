# Fibonacci
def fib(n):
    if n<2:
        return 1
    else:
        return fib(n-1)+fib(n-2)


# Fibonacci com memoization
def fib_memo(n):
    return aux(n,{})

def aux(n, memo):
    if n in memo:
        return memo[n]
    
    if n < 2:
        memo[n] = 1
        
    else:
        memo[n] = aux(n-1, memo) + aux(n-2, memo)
    
    return memo[n]

# Fibonacci com programação dinâmica
def fib_dynamic(n):
    memo = {}
    memo[0] = 1
    memo[1] = 1
    
    for i in range(2, n+1):
        memo[i] = memo[i-1] + memo[i-2]
    
    return memo[n] 

# Trocar
def trocar(valor,moedas):
    if valor == 0:
        return 0
    
    r = float("inf")
    for m in moedas:
        if m <= valor:
            r = min(r,1+trocar(valor-m,moedas))
            
    return r

# Trocar com memoization
def trocar_memo(valor,moedas):
    return aux_troca(valor,moedas,{})

def aux_troca(valor,moedas,d):
    if valor in d:
        return d[valor]
    
    if valor == 0:
        d[valor] = 0
        return 0
    
    r = float("inf")
    for m in moedas:
        if m <= valor:
            r = min(r,1+aux(valor-m,moedas,d))
            d[valor] = r
    
    return r

# Trocar com programação dinâmica
def trocar_dynamic(valor,moedas):
    d = {}
    d[0] = 0
    for v in range(1,valor+1):
        r = float("inf")
    
        for m in moedas:
            if m <= v:
                r = min(r,1+d[v-m])
    
        d[v] = r
        if v >= max(moedas):
            del d[v-max(moedas)]
    
    return d[valor]