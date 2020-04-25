import fileinput

# Class node to create BST


class Node:
    def __init__(self, data):
        self.left = None
        self.right = None
        self.data = data

#  BST class with basic operations of insertion and searching


class BST:
    def __init__(self):
        self.root = None

    # Set root element in input data
    def set_root(self, data):
        self.root = Node(data)

    # Insert new node to the tree
    def insert_node(self, data):
        # If root is none, or blank, set root to input elements
        if self.root is None:
            self.set_root(data)
        else:
            # Create a node of input data
            n = Node(data)
            # Initialize root element
            troot = self.root
            # While root element is not None or blank
            while troot:
                # If input element is less than root element
                if data < troot.data:
                    # insert node to left
                    if troot.left:
                        troot = troot.left
                    else:
                        troot.left = n
                        break
                # If input is greater than root element
                else:
                    # Insert node to right of root
                    if troot.right:
                        troot = troot.right
                    else:
                        troot.right = n
                        break

    # Search particular node in trees
    def search_node(self, data):
        # Output string
        output_string = 'found: '
        # If root is None or blank
        if self.root is None:
            print('not found')
            return
        # If root is not None or blank
        else:
            # Set root in troot variable
            troot = self.root
            # While root is not None or blank
            while troot:
                # If search value is less than root value, Move to left.
                if data < troot.data:
                    # If left node is not None or blank
                    if troot.left:
                        # Set first node on left to root
                        troot = troot.left
                        # Update traversal to string
                        output_string += 'l '
                        # If updated root is equal to input data, return the outputstring with traversal order
                        if troot.data == data:
                            return output_string
                    # If element is not present in left side
                    else:
                        return ("not found")
                # If search value is greater than root value, move to right.
                elif data > troot.data:
                    # If right node is not None or blank
                    if troot.right:
                        # Set first node on right to root
                        troot = troot.right
                        # Update traversal to string
                        output_string += 'r '
                        # If updated root is equal to input data, return the outputstring with traversal order
                        if troot.data == data:
                            return output_string
                    # If element is not present in right side
                    else:
                        return ("not found")
                # If search value is equal to root value, print root
                else:
                    return ("found: root")


# Initialize tree
tree = BST()
# For each line input from file
for line in fileinput.input():
    inputText = line
    # If line starts with i alphabet, means insert the element
    if inputText.startswith("i"):
        element = inputText.split(" ")[1]
        tree.insert_node(int(element))
    # If line starts with q, means search the element
    if inputText.startswith("q"):
        element = inputText.split(" ")[1]
        print(tree.search_node(int(element)))
