class Z:
    def __str__(self):
        return "Z"

class S:
    def __init__(self, p):
        self.p = p

    def __str__(self):
        return f"S({str(self.p)})"

Zero = Z()
Succ = S

show = lambda c: print(c(Succ)(Zero))

zero = lambda s: lambda z: z
one  = lambda s: lambda z: s(z)
succ = lambda n: lambda s: lambda z: s (n(s)(z))

plus = lambda m: lambda n: lambda s: lambda z: m(s)(n(s)(z))
mult = lambda m: lambda n: lambda s: lambda z: m(n(s))(z)

pair = lambda car: lambda cdr: lambda f: f(car)(cdr)

car = lambda pi: pi(lambda x: lambda y: x)
cdr = lambda pi: pi(lambda x: lambda y: y)

three = plus(plus(one)(one))(one)

zero_pair = pair(zero)(zero)
succ_pair = lambda pi: pair(succ(car(pi)))(car(pi))

pred = lambda n: cdr(n(succ_pair)(zero_pair))

minus = lambda n: lambda m: m(pred)(n)