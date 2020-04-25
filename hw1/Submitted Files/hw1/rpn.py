import fileinput


def performOperation(no1, no2, operationToPerform):
    if operationToPerform == "*":
        # print("Multiplication")
        return (no1 * no2)

    if operationToPerform == "+":
        # print("Addition")
        return (no1 + no2)

    if operationToPerform == "-":
        # print("Subtraction")
        return (no2 - no1)

    if operationToPerform == "/":
        # print("Division")
        return (no1 / no2)

    if operationToPerform == "~":
        # print("Negatation")
        return (no1 * (-1))


class Stack:
    def __init__(self):
        self.items = []

    def isEmpty(self):
        return self.items == []

    def push(self, item):
        self.items.append(item)

    def pop(self):
        return self.items.pop()

    def peek(self):
        return self.items[len(self.items)-1]

    def size(self):
        return len(self.items)


s = Stack()

# For each line input from file
for line in fileinput.input():
    line = line.strip('\n')
    line = line.strip('\t')
    if line == '+' or line == '*' or line == '-' or line == '/' or line == '~':
        s.push(performOperation(int(s.pop()), int(s.pop()), line))
        print(s.peek())
    else:
        s.push(int(line))
        print(int(line))
