/**
 * 
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
 * @author mstevens
 * 
 */
@SuppressWarnings("unused")
public class OrientationSensor implements SensorEventListener
{
	
	static protected final String TAG = "OrientationSensor";

	private SensorManager sensorManager;

	private float[] R;
	private float[] gravity;
	private float[] geomagnetic;
	private Orientation lastOrientation;
	private OrientationListener listener;

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
		// If the sensor data is unreliable return
		if(sensorEvent.accuracy == SensorManager.SENSOR_STATUS_UNRELIABLE)
			return;
		// Gets the value of the sensor that has been changed
		switch(sensorEvent.sensor.getType())
		{
			case Sensor.TYPE_ACCELEROMETER:
				//Log.d(TAG, "Accelerometer update");
				gravity = sensorEvent.values.clone();
				break;
			case Sensor.TYPE_MAGNETIC_FIELD:
				//Log.d(TAG, "Magnetic field update");
				geomagnetic = sensorEvent.values.clone();
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
