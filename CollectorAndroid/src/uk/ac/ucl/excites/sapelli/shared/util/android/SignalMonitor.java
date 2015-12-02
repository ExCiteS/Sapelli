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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.content.Context;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.TelephonyManager;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public class SignalMonitor extends PhoneStateListener
{
	
	static protected final String TAG = SignalMonitor.class.getSimpleName();
	
	static public SignalMonitor Start(Context context)
	{
		return new SignalMonitor(context);
	}

	private TelephonyManager telephonyManager;
	private int serviceState;
	private boolean roaming;
	private int signalStrength;

	/**
	 * Check if there is GSM connectivity. The serrviceState has 3 modes, 0 : Normal operation condition, the phone is registered with an operator either in
	 * home network or in roaming. 1 : Phone is not registered with any operator, the phone can be currently searching a new operator to register to, or not
	 * searching to registration at all, or registration is denied, or radio signal is not available. 3 : Radio of telephony is explicitly powered off.
	 */
	private SignalMonitor(Context context)
	{
		this.telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		telephonyManager.listen(this, PhoneStateListener.LISTEN_SERVICE_STATE | PhoneStateListener.LISTEN_SIGNAL_STRENGTHS);
	}

	@Override
	public synchronized void onServiceStateChanged(ServiceState service)
	{
		serviceState = service.getState();
		roaming = service.getRoaming();
	}

	@Override
	public synchronized void onSignalStrengthsChanged(SignalStrength signalStr)
	{
		if(signalStr.isGsm())
			signalStrength = signalStr.getGsmSignalStrength();
	}

	/**
	 * @return the serviceState
	 */
	public int getServiceState()
	{
		return serviceState;
	}

	/**
	 * @return whether the device is registered to a network (but perhaps with low signal strength) 
	 * @see http://stackoverflow.com/a/3427615/1084488
	 */
	public boolean isInService()
	{
		return serviceState == ServiceState.STATE_IN_SERVICE && signalStrength >= 0 && signalStrength <= 31;
	}

	/**
	 * @return the roaming
	 */
	public boolean isRoaming()
	{
		return roaming;
	}

	/**
	 * @return the signalStrength
	 */
	public int getSignalStrength()
	{
		return signalStrength;
	}

	public void stop()
	{
		telephonyManager.listen(this, PhoneStateListener.LISTEN_NONE);
	}
	
}
