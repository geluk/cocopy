
limit: int = 0
current: int = 1
previous: int = 0
next: int = 0
limit = input()

while current <= limit:
    print(current)
    next = current + previous
    previous = current
    current = next
