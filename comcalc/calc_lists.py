def fact(n):
    retval = ""
    n0 = 1.0
    for i in range(1,n):
        n0 *= float(i)
        retval += str(n0) + ".0;"
    print retval
fact(20)
