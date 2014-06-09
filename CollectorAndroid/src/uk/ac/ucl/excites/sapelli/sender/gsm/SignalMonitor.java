package uk.ac.ucl.excites.sapelli.sender.gsm;

import uk.ac.ucl.excites.sapelli.util.Debug;
import android.content.Context;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.TelephonyManager;

public class SignalMonitor extends PhoneStateListener
{
	static protected final String TAG = "SignalMonitor";

	private TelephonyManager telephonyManager;
	private int serviceState;
	private boolean roaming;
	private int signalStrength;

	/**
	 * Check if there is GSM connectivity. The serrviceState has 3 modes, 0 : Normal operation condition, the phone is registered with an operator either in
	 * home network or in roaming. 1 : Phone is not registered with any operator, the phone can be currently searching a new operator to register to, or not
	 * searching to registration at all, or registration is denied, or radio signal is not available. 3 : Radio of telephony is explicitly powered off.
	 */
	public SignalMonitor(Context context)
	{
		this.telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
		telephonyManager.listen(this, PhoneStateListener.LISTEN_SERVICE_STATE | PhoneStateListener.LISTEN_SIGNAL_STRENGTHS);
	}

	@Override
	public synchronized void onServiceStateChanged(ServiceState service)
	{
		serviceState = service.getState();
		roaming = service.getRoaming();

		Debug.d("Service changed to: " + serviceState);
	}

	@Override
	public synchronized void onSignalStrengthsChanged(SignalStrength signalStr)
	{

		Debug.d("Signal changed to: " + signalStr);

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

	public boolean isInService()
	{
		return serviceState == ServiceState.STATE_IN_SERVICE;
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

	public void stopSignalMonitor()
	{
		telephonyManager.listen(this, PhoneStateListener.LISTEN_NONE);
	}
}
