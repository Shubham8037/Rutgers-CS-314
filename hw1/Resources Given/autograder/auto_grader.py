#!/usr/bin/env python3

import os
import sys
import time
import signal
import subprocess

basepath = 'hw1'
problems = ['matmult', 'bst', 'rpn', 'dfa']
buildRequired = False
problemsInSeparateDirectories = False
test_cases_directory = os.getcwd() + '/testcases/'

maxPossibleScore = 0 # calculated automatically

# if a program has single-liner input and output, we put all test
# cases in single file. Otherwise, we have a file for test and
# associated file with results
formats = {'matmult': 'file',
           'bst': 'file',
           'rpn': 'file',
           'dfa': 'file'}

# value for each test case for each problem
weight = {'matmult': 5.0,
          'bst': 5.0,
          'rpn': 5.0,
          'dfa': 5.0}

# only used to determine when to use floating point comparisons
valueTypes = {'matmult': 'float',
              'bst': 'string',
              'rpn': 'float',
              'dfa': 'string'}
class ExperimentError(Exception):
    def __init__(self, command, output):
        self.command = command
        limit = 10000
        if len(output) > limit:
          self.output = output[:limit/2] \
                      + '\n\n...TRUNCATED...\n\n' \
                      + output[-limit/2:]
        else:
          self.output = output
    def __str__(self):
        return 'ExperimentError:' + repr(self.command)

def run_command(command_string, inputFile=None, max_lines=0, throw_exception=True, user_program=False):
    if inputFile:
        with open(inputFile) as f:
            obj = subprocess.Popen(command_string, stdin=f, stdout=subprocess.PIPE)
    else:
        obj = subprocess.Popen(command_string, stdout=subprocess.PIPE)
    output = ''
    maxSleep = 20
    if user_program: #program may have an infinite loop
        while maxSleep > 0:
            time.sleep(0.25)
            maxSleep -= 1
            if obj.poll() != -1:
                break
        if maxSleep == 0 and obj.poll() == -1:
            os.kill(obj.pid, signal.SIGKILL)
            print(command_string + ' taking longer than expected. Killed.')
            return ''
    for line in obj.stdout:
        output += line.decode()
    exit_status = obj.wait()
    if max_lines != 0:
        lines = output.split('\n')
        output = string.join(lines[-max_lines:], '\n')

    if throw_exception and exit_status != 0:
        raise ExperimentError(command_string, output)

    return output

def compare_string_file(correctFile, actualString, floats=False):
    actualLines = actualString.split('\n')
    i = 0
    flag = True
    with open(correctFile) as fd:
        for correctLine in fd:
            if i < len(actualLines):
                if not compare_string(correctLine, actualLines[i], floats):
                    flag = False
            elif len(correctLine.strip()) > 0:
                print('Output missing: ' + correctLine.strip())
                flag = False
            i += 1
    while i < len(actualLines):
        if len(actualLines[i].strip()) == 0:
            i += 1
            continue
        print('Extra output: ' + actualLines[i])
        i += 1
        flag = False
    return flag

def compare_floats(f1, f2, s1, s2):
    if len(f1) != len(f2):
        print('Expected: ' + s1)
        print('Observed: ' + s2)
        return False

    for i in range(len(f1)):
        if abs(f1[i] - f2[i]) >= 1e-6:
            print('Expected: ' + s1)
            print('Observed: ' + s2)
            return False
    return True

# A bit of a hack, but some problems that have float values
# may also need to output error strings on some inputs
def looksLikeFloats(s2):
    try:
        [float(x) for x in s2.split()]
        return True
    except ValueError:
        return False

def compare_string(s1, s2, floats=False):
    s1 = s1.strip()
    s2 = s2.strip()

    if floats and looksLikeFloats(s2):
        f1 = [float(x) for x in s1.split()]
        f2 = [float(x) for x in s2.split()]
        return compare_floats(f1, f2, s1, s2)

    if s1 == s2:
        return True
    elif s1.lower() == s2.lower():
        print('%s and %s are in different case. Please print your output in correct case.' % (s1, s2))
    else:
        print('Expected: ' + s1.strip())
        print('Observed: ' + s2.strip())
    return False

def make_executable(dirname):
    if os.path.isfile('Makefile') or os.path.isfile('makefile'):
        run_command(['make', 'clean'])
        run_command(['make'])
    else:
        print('No Makefile found in ' + dirname)
        print('Please submit a Makefile to receive full grade.')
        run_command(['gcc', '-Wall', '-Werror', '-fsanitize=address', '-o', dirname, '*.c', '*.h'])

def buildProject(dirname):
    if not buildRequired:
        return True

    try:
        make_executable(dirname)
    except:
        print('An exception occured trying to build ' + dirname)
        print('Score is %d\n' % score)
        return False

    if not os.path.isfile(dirname):
        print('Executable %s missing. Please check the compilation output.' % dirname)
        print('Score is %d\n' % score)
        return False

    return True

def gradeFileInput(dirname):
    print('Grading ' + dirname)
    prevdir = os.getcwd()
    if problemsInSeparateDirectories:
        os.chdir(dirname)
    score = 0

    if not buildProject(dirname):
        os.chdir(prevdir)
        return

    test_dir = test_cases_directory + dirname  + '/'

    for testfile in sorted(os.listdir(test_dir)):
        if os.path.isdir(testfile) or not testfile.startswith('test'):
            continue
        resultfile = test_dir + 'result' + testfile[4:len(testfile)]
        if not os.path.isfile(resultfile):
            print('Found a test file %s, but no associated result file.' % testfile)
            continue

        try:
            global maxPossibleScore
            maxPossibleScore += weight[dirname]
#            command_str = [dirname, test_dir + testfile]
            command_str = ['python3', dirname + '.py']
            inputFile = test_dir + testfile
            ret = run_command(command_str, user_program=True, inputFile=inputFile)
            floats = True if valueTypes[dirname] == 'float' else False
            if compare_string_file(resultfile, ret, floats):
                score += weight[dirname]
            else:
                print('The output is not correct for input file ' + testfile)
        except:
            print('An exception occurred while executing %s %s' % (dirname, testfile))

    print('Score is %d\n' % score)
    scores[dirname] = score
    if problemsInSeparateDirectories:
        os.chdir(prevdir)

def gradeLineInput(dirname):
    print('Grading ' + dirname)
    prevdir = os.getcwd()
    if problemsInSeparateDirectories:
        os.chdir(dirname)
    score = 0

    if not buildProject(dirname):
        if problemsInSeparateDirectories:
            os.chdir(prevdir)
        return

    test_file = test_cases_directory + dirname + '/test.txt'

    if not os.path.isfile(test_file):
        print('Expecting the test cases in test.txt. Not found.')
        print('Score is %d\n' % score)
        if problemsInSeparateDirectories:
            os.chdir(prevdir)
        return
    else:
        print('')

    with open(test_file) as fd:
        for line in fd:
            maxPossibleScore += weight[dirname]
            inputline = line
            outputline = next(fd)
            try:
                command_str = [dirname, inputline]
                ret = run_command(command_str, user_program=True)
                floats = True if valueTypes[dirname] == 'float' else False
                if compare_string(outputline, ret, floats):
                    score += weight[dirname]
                else:
                    print('The output is not correct for input ' + inputline)
            except:
                print('An exception occured trying to execute ' + ' '.join(command_str))

    print('Score is %d\n' % score)
    scores[dirname] = score
    if problemsInSeparateDirectories:
        os.chdir(prevdir)

def global_grade(dirname):
    for p in problems:
        if problemsInSeparateDirectories and not os.path.isdir(os.path.join(p)):
            continue
        if p in formats and formats[p]=='line':
            gradeLineInput(p)
        elif p in formats and formats[p]=='file':
            gradeFileInput(p)
        else:
            print('Error: no format specified for problem ' + p)

def main():
    global scores
    scores = {}
    for p in problems:
        scores[p] = 0

    tarmode = False
    if len(sys.argv) > 1:
        if sys.argv[1].strip().endswith('tar'):
            tarmode=True

    if not tarmode:
        if not os.path.isdir(basepath):
            print(basepath + ' is not present in this directory.')
            sys.exit(1)
        else:
            print('Grading the content of ' + basepath)
            os.chdir(basepath)
            global_grade(basepath)
    else:
        prevdir = os.getcwd()
        if not os.path.exists(basepath + '.tar'):
            print('Expecting %s.tar in current directory (%s)' % (basepath, prevdir))
            print('Please make sure you created %s.tar in the right directory' % basepath)
            sys.exit(1)
        if os.path.exists('obj_temp'):
            print('Deleting the directory obj_temp.')
            run_command(['rm', '-rf', 'obj_temp'])
        run_command(['mkdir', 'obj_temp'])
        os.chdir('obj_temp')
        run_command(['tar', '-xvf', '../%s.tar' % basepath])
        if os.path.isdir(basepath):
            os.chdir(basepath)
            global_grade(basepath)
        else:
            print('There is no directory named %s in %s.tar.' % (basepath, basepath))
            print('Please check your tar file.')
        os.chdir(prevdir)
    totalScore = 0.0
    print('You scored ')
    for p in problems:
        totalScore += scores[p]
        print('%s: %d' % (p, scores[p]))

    print('Your total score = %d / %d' % (totalScore, maxPossibleScore))

if __name__ == '__main__':
    main()
