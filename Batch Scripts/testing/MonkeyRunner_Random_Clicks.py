from com.android.monkeyrunner import MonkeyRunner, MonkeyDevice
from random import randint
import sys

print ""
print "System info:"
print sys.version
print ""

print "Connecting to Device."
device = MonkeyRunner.waitForConnection();
# Get screen size
DeviceX = int(device.getProperty("display.width"));
DeviceY = int(device.getProperty("display.height"));
print ""
print "Screen size: " + str(DeviceX) + "x" + str(DeviceY);
print ""

x = input('What is your name?  ');
print ('Your name is ' + x);

# with your activity opened start the monkey test
print "Start Monkey Test:"
for i in range(1, 1000000):
    # emulate only simple touches: touch ( integer x, integer y, string type)
	randomX = randint(0, DeviceX);
	randomY = randint(0, DeviceY);
	print "Attempt: " + str(i) + ", clicking on: "+ str(randomX) + ", " + str(randomY);
	device.touch(randomX, randomY, 'DOWN_AND_UP');
	# sleep for 1 second 
	MonkeyRunner.sleep(0.5)

print "End Monkey Test."