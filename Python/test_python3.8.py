# class State:
#     def __init__(self, env={}):
#         self.env = env
    
#     def Set(self, name, value):
#         self.env[name] = value

#     def Get(self, name):
#         return self.env[name]

# def newState(s=None):
#     return State() if s is None else State(s)

# def Begin(*args): return args[-1]

# # Begin(
# #     x := int(input()),
# #     y := int(input()),
# #     x + y
# # )

# # Begin(
# #     func := lambda x, y: Begin(
# #         z := int(input()),
# #         x + y + z
# #     ),
# #     func(1, 2)
# # )

# # Begin(
# #     (func := (lambda x: 1 if x <= 1 else x + func(x - 1))),
# #     print(func(10)))

# def Unless(cond): return lambda body: body if not cond else lambda _=None: None
# def While(env, cur, cond): 
#     def func(body, cur=cur):
#         if cond(cur):
#             if (cur := body(env, cur)) is None:
#                 return None
#             else:
#                 return func(body, cur)
#     return func

# print(Begin(
#     print(1),
#     x := 1,
#     y := 2,
#     print(x + y),
#     x * y
# ))

# class StateT:
#     def __init__(self, func):
#         self.func = func
    
#     def __gt__(self, func):
#         return bindState(self, func.func)
    
#     def __mul__(self, o):
#         return o

# def someTrivialFunc(x): return sum(range(x))
# def printIfGe(x, lo):
#     if (s := someTrivialFunc(x)) >= lo:
#         print(s)
#     else:
#         print('Too small')

# def whatTheHeck(a, b, c):
#     print(a, b, c)
# whatTheHeck(x := 1, y := x + 1, z := y + 1)

# printIfGe(10, 20)

# def moreAndMore(a, b, *others):
#     print(a, b, others)

# state = lambda func: StateT(func)
# get = lambda: state(lambda state: (state, state))
# put = lambda newState: state(lambda _: (None, newState))
# returnState = lambda state: StateT(lambda x: (x, state))
# runState = lambda stateT: lambda st: stateT.func(st)
# bindState = lambda oldState, step: StateT(lambda state: Begin(
#     pair := oldState.func(state),
#     print(pair),
#     result := pair[0],
#     st := pair[1],
#     newState := step(result),
#     newState.func(st)
# ))
# dropRight = lambda left: lambda right: right
# evalState = lambda state, start: state.func(start)

# def testStateT():
#     empty = []
#     push = lambda x: state(lambda st: ((), Begin(
#         new := st.copy(),
#         new := [ x ] + new,
#         new
#     )))
#     pop = lambda st: Begin(
#         xs := st.copy(),
#         x  := xs[0],
#         xs := xs[1:],
#         (x, xs)
#     )

# # Begin(testIte := 
# #             Begin(
# #                 x := int(input()),
# #                 y := int(input()),
# #                 (Ite(x > y)(
# #                     x
# #                 )(
# #                     y
# #                 ))
# #             ),
# #     print(testIte))
# Begin(testIteOK := 
#             Begin(
#                 x := None,
#                 y := 1,
#                 Ite(x is not None)(
#                     lambda: x + y
#                 )(
#                     lambda: y
#                 )
#             ))
import math

Z = lambda f: (lambda x: f (lambda y: x(x)(y)))(lambda x: f (lambda y: x(x)(y)))

Begin(
    almost_factorial := lambda f: lambda x: 1 if x == 0 else x * f(x - 1),
    factorial := Z(almost_factorial),
    result := factorial(10),
    result
)

Begin = lambda *x: x[-1]
Return = lambda *x: lambda: Begin(*[(None, ) + x])
Pass = Return
def Ite(cond): return lambda ifBranch: lambda elseBranch: ifBranch() if cond else elseBranch()
def When(cond): return lambda body: body() if cond else None

Begin(
    func := lambda beg, end, f: Begin(
        Ite(beg > end)(
            lambda: True
        )(
            lambda: f(beg) and func(beg + 1, end, f)
        )
    ),
    isPrime := lambda x: Begin(
        Ite(x < 2)(
            lambda: False
        )(
            lambda: Ite(x == 2)(
                        lambda: True
                    )(
                        lambda: func(2, x ** 0.5, lambda i: x % i != 0)
                    )
        )
    ),
    printPrime := lambda x: Begin(
        print(x),
        func := lambda dep, end: 
                Ite(dep > end)(
                    Return(0)
                )(
                    lambda: Begin(
                                When(isPrime(dep))(
                                    lambda: print(f'{dep} is a prime')
                                ),
                    func(dep + 1, end))
                ),
        func(2, x)
    ),
    printPrime(50)
)

Begin(
    (func := (lambda x: 1 if x <= 1 else x + func(x - 1))),
    (someValue :=
        (Begin(
            x := 1,
            y := 2,
            When(x < y)(
                lambda _=None: x + y
            )
        ))),
    (testWhile := 
        Begin(
            i := 0,
            state := newState({'sum' : 0, 'evens': []}),
            upper := int(12),
            While(state, i, lambda x: x <= upper)(
                lambda env, cur: Begin(
                    env.Set('sum', cur + env.Get('sum')),
                    cont := When(cur % 2 == 0)(
                        Begin(
                            cur_env := env.Get('evens'),
                            lambda _=None: Begin(
                                            cur_env.append(cur),
                                            env.Set('evens', cur_env)))
                    )(),
                    cur + 1
                )
            ),
            state.Get('evens')
        )),
    (testIte := Begin(
        x := int(input()),
        y := int(input()),
        (Ite(x > y)(
            x
        )(
            y
        ))
    )),
    (testIteNotGood := Begin(
        x := None,
        y := 1,
        Ite(x is not None)(
            x + y
        )(
            y
        )()
    )),
    print(func(10)),
    print(someValue),
    print(testWhile),
    print(testIte),
    print(testIteNotGood))