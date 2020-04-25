import fileinput
import numpy as np


def CheckAndReturnValue(element):
    if "." in element:
        return float(element)
    else:
        return int(element)


def print_matrix(matrix):
    for i in range(len(matrix)):
        print(' '.join(map(str, matrix[i])))


countLine = 0
matrix1 = False
matrix2 = False
isCalulatedFirstMatrix = False
isCalulatedSecondMatrix = False
array1 = []
array2 = []

for line in fileinput.input():

        # Set first matrix dimensions
    if fileinput.isfirstline():
        matrix1 = True
        rows1 = int(line.split(' ')[0])
        cols1 = int(line.split(' ')[1])
        countLine = 1

    elif not fileinput.isfirstline() and countLine <= rows1 and not isCalulatedFirstMatrix:

        #     Lock this condition for further lines
        if countLine == rows1:
            isCalulatedFirstMatrix = True

        # Remove special characters
        line = line.strip('\n')
        line = line.strip('\t')
        # Split the numbers by spaces
        lineList = line.split(' ')
        # Remove empty elements
        lineList = [x for x in lineList if x]
        # Map to integer
        lineList = list(map(float, lineList))
        # Append array
        array1.append(lineList)
        # Increment the count of line
        countLine = countLine + 1

    elif not matrix2 and isCalulatedFirstMatrix:

        countLine = 1
        matrix2 = True
        rows2 = int(line.split(' ')[0])
        cols2 = int(line.split(' ')[1])

    elif countLine <= rows2 and not isCalulatedSecondMatrix:

        #     Lock this condition for further lines
        if countLine == rows2:
            isCalulatedFirstMatrix = True
        # Remove special characters
        line = line.strip('\n')
        line = line.strip('\t')
        # Split the numbers by spaces
        lineList = line.split(' ')
        # Remove empty elements
        lineList = [x for x in lineList if x]
        # Map to integer
        lineList = list(map(float, lineList))
        # Append array
        array2.append(lineList)
        # Increment the count of line
        countLine = countLine + 1


if(cols1 != rows2):
    print("invalid input")
    exit()

result = [[0 for j in range(0, cols2)] for i in range(0, rows1)]

# for multiplication
# i will run throgh each row of matrix1
for i in range(0, rows1):
    # j will run through each column of matrix 2
    for j in range(0, cols2):
        # k will run throguh each row of matrix 2
        for k in range(0, cols1):
            result[i][j] += array1[i][k] * array2[k][j]

print_matrix(result)
