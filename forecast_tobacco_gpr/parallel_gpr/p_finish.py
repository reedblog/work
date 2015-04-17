# just a script that will make a file after some jobs are finished
import sys
path = sys.argv[1]

f = open('{path:s}/finished.txt'.format(path = path), 'w')

f.write('YOURE DONE!!')

f.close()
