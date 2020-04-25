import fileinput


class DFA:
    current_state = None

    def __init__(self, states, symbols, transition_function, start_state, accept_states):
        self.states = states
        self.symbols = symbols
        self.transition_function = transition_function
        self.start_state = start_state
        self.accept_states = accept_states
        self.current_state = start_state
        return

    def transition_to_state_with_input(self, input_value):
        if ((self.current_state, input_value) not in self.transition_function.keys()):
            self.current_state = None
            return
        self.current_state = self.transition_function[(
            self.current_state, input_value)]
        return

    def in_accept_state(self):
        return self.current_state in accept_states

    def go_to_initial_state(self):
        self.current_state = self.start_state
        return

    def run_with_input_list(self, input_list):
        self.go_to_initial_state()
        for inp in input_list:
            self.transition_to_state_with_input(inp)
            continue
        return self.in_accept_state()
    pass


isInputStarted = False
start_state = None
accept_states = None

for line in fileinput.input():

    line = line.strip('\n')

    # Set Start State
    if line.split(':')[0] == 'start':
        start_state = line.split(':')[1].strip()

    # Set Final States
    if line.split(':')[0] == 'final':
        accept_states = ''
        splitSymbols = line.split(':')
        for i in splitSymbols:
            if ' ' in i.strip():
                accept_states = i.strip().split(' ')
            else:
                accept_states = splitSymbols[1].strip()

    # Supply inputs to program
    if isInputStarted:

        states = {'q0', 'q1', 'q2', 'q3'}
        symbols = {'a', 'b', 'c', '0', '1'}

        tf = dict()
        tf[('q0', 'a')] = 'q1'
        tf[('q0', 'b')] = 'q3'
        tf[('q0', 'c')] = 'q2'
        tf[('q1', 'a')] = 'q1'
        tf[('q1', 'b')] = 'q2'

        tf[('q0', '1')] = 'q0'
        tf[('q0', '0')] = 'q1'
        tf[('q1', '1')] = 'q0'
        tf[('q1', '0')] = 'q1'

        d = DFA(states, symbols, tf, start_state, accept_states)

        inp_program = list(line)

        if d.run_with_input_list(inp_program) is True:
            print("accepted")
        else:
            print("rejected")

    if line.split(":")[0] == 'final':
        isInputStarted = True
