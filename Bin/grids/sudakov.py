#!/usr/bin/env python3

import os
import subprocess
import numpy as np

#
# set up for subprocess 
#

# main directory of jetvheto
jetvheto = '/home/luke/Documents/Physics/Research/JetVHeto-RE'

#open /dev/null as output location
DEVNULL = open(os.devnull, 'w')

def runJetVHeto(loc, proc, M, order, xmuR=0.5, xmuF=0.5, xQ=0.5):
	
	#command to be run
	cmd = ['./jetvheto', '-resum-only', '-proc', str(proc), '-collider', 'pp', '-sqrts', '13000', '-pdf_name', 'PDF4LHC15_nlo_mc','-order', str(order),'-nbins', '100', '-ptmin', '1','-ptmax','100', '-M', str(M),'-xmuR', str(xmuR), '-xmuF', str(xmuF), '-xQ', str(xQ)]
	
	#run jetvheto
	#set errors (which is the main text in this case) to devnull
	#pipe output 
	process = subprocess.Popen(cmd, cwd=loc, universal_newlines=True, stdout=subprocess.PIPE, stderr=DEVNULL)
	process.wait()

	#get jetvheto output
	stdout_value = process.communicate()[0]

	#start of interesting data
	head = '# the lines that follow give the jet-veto efficiencies as a function of pt'
	end = 'Thanks for using LHAPDF 6.1.6. Please make sure to cite the paper:'

	#extract sudakov factor from piped output
	input_data = stdout_value.split('\n')
	
	#build sudakov matrix
	#if jetVHeto cannot extrapolate down to a given MWW it is replaced by
	# 1.0, this makes sense based on what I have tried to do.
	try:
		input_trim = input_data[input_data.index(head)+2:input_data.index(end)]
		tmp = [x.split('   ') for x in input_trim]
		sudakov = [[float(M), 0.0, 0.0]]
	
		for x in tmp[:len(tmp)]:
			sudakov.append([float(M), float(x[1]), float(x[3])])
	except:
		sudakov = [[M, 0.0, 0.0]]
		
		for x in range(1,101):
			sudakov.append([float(M), float(x), 1.0])

	

	return sudakov


#test scales
#scales = [[0.5,0.5,0.5]]

scales = [ [0.25,0.25,0.5], [0.25,0.5,0.5], [0.5,0.25,0.5], [0.5,0.5,0.5], [0.5,1.0,0.5], [1.0,0.5,0.5], [1.0,1.0,0.5], [0.5,0.5,1.0] ]

#
# the range of values tabluated can be changed as desired by the user,
# just be sure to make sure the grid shape remains (101, 101, 3)
#

for scale, facscale, resumscale in scales:
	sudakov = []

	for mww in range(0,1010,10):
		sudakov.append(runJetVHeto(jetvheto, 'DY', mww, 2, scale, facscale, resumscale))
	
	gridname = "muR=" + '{:4.2f}'.format(scale) + "_muF=" + '{:4.2f}'.format(facscale) + "_muQ=" + '{:4.2f}'.format(resumscale) + ".bin"	
	sudakov = np.array(sudakov)

	with open(gridname, "wb") as output:
		(sudakov.T).tofile(output)
