/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.geo;

import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.util.Log;

/**
 * @author mstevens, Julia
 * 
 */
public class OrientationSensor implements SensorEventListener
{
	
	static protected final String TAG = "OrientationSensor";
	static protected final int MAX_TRIES = 5;

	private SensorManager sensorManager;

	private float[] R;
	private float[] gravity;
	private float[] geomagnetic;
	private Orientation lastOrientation;
	private OrientationListener listener;
	private int gravityTries;
	private int geomagneticTries;
	
	public OrientationSensor(Context context)
	{
		sensorManager = (SensorManager) context.getSystemService(Context.SENSOR_SERVICE);
	}

	public void start(OrientationListener listener)
	{
		this.listener = listener;
		
		R = new float[16];
		gravity = null;
		geomagnetic = null;
		gravityTries = 0;
		geomagneticTries = 0;
		
		// Register this class as a listener for the accelerometer sensor
		sensorManager.registerListener(this, sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER), SensorManager.SENSOR_DELAY_NORMAL);
		// ...and the orientation sensor
		sensorManager.registerListener(this, sensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD), SensorManager.SENSOR_DELAY_NORMAL);
	}

	public void stop()
	{
		sensorManager.unregisterListener(this);
	}

	/**
	 * Uses:
	 *  - http://developer.android.com/reference/android/hardware/SensorManager.html#getRotationMatrix(float[], float[], float[], float[])
	 *  - http://developer.android.com/reference/android/hardware/SensorManager.html#getOrientation(float[], float[])
	 *  
	 * Some related discussions:
	 *  - http://developer.android.com/guide/topics/sensors/sensors_position.html#sensors-pos-orient
	 *  - http://stackoverflow.com/q/4819626/1084488
	 *  - http://stackoverflow.com/q/4174389/1084488
	 *  - http://stackoverflow.com/q/10032374/1084488
	 *  
	 * @see android.hardware.SensorEventListener#onSensorChanged(android.hardware.SensorEvent)
	 */
	@Override
	public void onSensorChanged(SensorEvent sensorEvent)
	{
		// Get the value of the sensor that has been changed, and evaluate accuracy
		switch(sensorEvent.sensor.getType())
		{
			case Sensor.TYPE_ACCELEROMETER:
				if(checkAccuracy("Accelerometer", ++gravityTries, sensorEvent))
					gravity = sensorEvent.values.clone();
				else
					return;
				break;
			case Sensor.TYPE_MAGNETIC_FIELD:
				if(checkAccuracy("Magnetic field", ++geomagneticTries, sensorEvent))
					geomagnetic = sensorEvent.values.clone();
				else
					return;
				break;
		}
		
		// If gravity and geomagnetic have values then find rotation matrix
		if(gravity != null && geomagnetic != null)
		{
			// checks that the rotation matrix is found
			boolean success = SensorManager.getRotationMatrix(R, null, gravity, geomagnetic);
			if(success)
			{
				float[] orientation = new float[3];
				SensorManager.getOrientation(R, orientation);
				orientation[0] = (float) Math.toDegrees(orientation[0]);
				orientation[1] = (float) Math.toDegrees(orientation[1]);
				orientation[2] = (float) Math.toDegrees(orientation[2]);
				
				//Normalise azimuth:
				orientation[0] = (orientation[0] + 360) % 360;
				//Normalise pitch (doesn't seem to have any effect):
				if(orientation[1] <= -90)
					orientation[1] += (-2 * (90 + orientation[1]));
				else if(orientation[1] >= 90)
					orientation[1] += (2 * (90 - orientation[1]));
				
				lastOrientation = new Orientation(orientation[0], orientation[1], orientation[2]); 
				listener.onOrientationChanged(lastOrientation);
			}
		}
	}
	
	/**
	 * Previously we would never use sensor values when the reported accuracy was SENSOR_STATUS_UNRELIABLE. However,
	 * some devices (especially Nexus4), report accelerometer accuracy as being SENSOR_STATUS_UNRELIABLE (almost) all
	 * the time, meaning we never got to compute the orientation.
	 * We therefore decided to wait for better sensor values up to MAX_TRIES times. If the values are still unreliable
	 * the MAX_TRIES-th time we use them anyway (but logging is used to report about unreliability).
	 * 
	 * Related info:
	 * - http://stackoverflow.com/questions/9487747/type-magnetic-field-event-not-firing-on-my-galaxy-nexus-ics-4-0-2
	 * - http://stackoverflow.com/questions/6256256/android-compass-seems-unreliable
	 * 
	 * @param tries
	 * @param sensorEvent
	 * @return whether or not to accept (i.e. use) the sensor values
	 */
	private boolean checkAccuracy(String sensorName, int tries, SensorEvent sensorEvent)
	{
		Log.d(TAG, sensorName + " update (#" + tries + ")");
		if(sensorEvent.accuracy == SensorManager.SENSOR_STATUS_UNRELIABLE)
		{	
			if(tries < MAX_TRIES)
			{
				Log.w(TAG, " " + sensorName + " values are unreliable, waiting for improvement...");
				return false;
			}
			else
				Log.w(TAG, " " + sensorName + " values are unreliable, but accepted anyway."); 
		}
		return true;
	}

	/**
	 * @return the lastOrientation
	 */
	public Orientation getLastOrientation()
	{
		return lastOrientation;
	}

	@Override
	public void onAccuracyChanged(Sensor sensor, int accuracy)
	{
		// do nothing
	}

}
