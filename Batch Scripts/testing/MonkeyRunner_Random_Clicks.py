# Script that simulates random click interactions on an Android Device
# arguments: 
# 	-i iterations: how many times is the script run
# 	-f frequency: interaction frequency in milliseconds
#
# Written by Michalis Vitos

# Imports
from com.android.monkeyrunner import MonkeyRunner, MonkeyDevice
from random import randint
import sys,getopt

# Print system Info
print ""
print "System info:"
print sys.version
print ""

def main(argv):

	# Get arguments
	iterations = 1000
	frequency = 100
	
	try:
		opts, args = getopt.getopt(argv,"hi:f:",["iterations=","frequency="])
	except getopt.GetoptError:
		print 'MonkeyRunner_Random_Clicks.py -i <iterations> -f <frequency>'
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-h':
			print 'MonkeyRunner_Random_Clicks.py -i <iterations> -f <frequency>'
			sys.exit()
		elif opt in ("-i", "--iterations"):
			iterations = int(arg)
		elif opt in ("-f", "--frequency"):
			frequency = float(arg)
			frequency = frequency / 1000

	# Connect to device
	print "Connecting to Device."
	device = MonkeyRunner.waitForConnection(30)
	
	# sleep for 1 second
	MonkeyRunner.sleep(2)
	
	# Get screen size
	DeviceX = int(device.getProperty("display.width"))
	DeviceY = int(device.getProperty("display.height"))
	print ""
	print "Screen size: " + str(DeviceX) + "x" + str(DeviceY)
	print ""

	# With your activity opened start the monkey test
	print "Start Monkey Test:"
	print ""
	print 'The script is going to run for ' + str(iterations) + " times, and every " + str(frequency) + " seconds"
	print ""

	for i in range(1, iterations):
		# emulate only simple touches: touch ( integer x, integer y, string type)
		randomX = randint(0, DeviceX)
		randomY = randint(0, DeviceY)
		print "Attempt: " + str(i) + ", clicking on: "+ str(randomX) + ", " + str(randomY)
		device.touch(randomX, randomY, 'DOWN_AND_UP');
		# sleep for x seconds
		MonkeyRunner.sleep(frequency)

	print "End Monkey Test."

if __name__ == "__main__":
	main(sys.argv[1:])