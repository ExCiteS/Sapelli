/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import java.io.File;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.view.View;
import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public class ImageFileItem extends Item
{

	private File file;
	
	public ImageFileItem(File file)
	{
		this.file = file;
	}

	@Override
	protected View getView(Context context)
	{
		
		
		//FileHelpers.getFileExtension(file).
		
		ImageView imageView = new ImageView(context);
		imageView.setImageBitmap(BitmapFactory.decodeFile(file.getAbsolutePath()));
		return imageView;
	}

}
