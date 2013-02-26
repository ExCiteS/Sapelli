package uk.ac.excites.sender;

import android.app.Activity;
import android.content.Intent;
import android.content.Intent.ShortcutIconResource;
import android.os.Bundle;

public class ShortcutCreator extends Activity
{
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		Intent shortcutIntent = new Intent(this, SenderBackgroundActivity.class);
		ShortcutIconResource iconResource = Intent.ShortcutIconResource.fromContext(this, R.drawable.ic_launcher);

		// The result we are passing back from this activity
		Intent intent = new Intent();
		intent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, shortcutIntent);
		intent.putExtra(Intent.EXTRA_SHORTCUT_NAME, "Service Settings");
		intent.putExtra(Intent.EXTRA_SHORTCUT_ICON_RESOURCE, iconResource);
		setResult(RESULT_OK, intent);

		finish();
	}

}
