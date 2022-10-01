
def make_int() -> int:
    return 10


def double(z : int) -> int:
    return z * 2

x : int = 0
x = make_int()

while x < 100:
    x = double(x)
    print(x)
