def squaresTo(n):
    """
    Returns a list of the square from 0 to n-1
    """
    result = []
    for k in range (n):
        result.append(k**2)
    return result

def squaresTo2(n):
    return [k**2 for k in range(n)]

def main():
    print (squaresTo(100))
    print (squaresTo2(100))

if  __name__ ==  "__main__":
    main()

