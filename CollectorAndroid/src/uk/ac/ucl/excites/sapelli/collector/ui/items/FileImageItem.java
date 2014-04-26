/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.io.File;
import java.io.FileInputStream;

import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import android.annotation.SuppressLint;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.widget.ImageView;

//import com.caverock.androidsvg.SVG;
import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * An ImageItem subclass for images stored as files
 * 
 * @author mstevens
 */
public class FileImageItem extends ImageItem
{
	
	static private final String TAG = "FileImageItem";

	private File file;
	
	public FileImageItem(File file)
	{
		this(null, file);
	}
	
	@SuppressLint("DefaultLocale")
	public FileImageItem(Long id, File file)
	{
		super(id, FileHelpers.getFileExtension(file.getName()).toLowerCase().startsWith("svg")); //will also work for svgz files
		this.file = file;
	}
	
	@Override
	protected void setImage(ImageView view)
	{
		try
		{
			if(!isVectorBased())
			{	// Raster image (PNG, JPG, GIF, ...):
				view.setImageBitmap(BitmapFactory.decodeFile(file.getAbsolutePath()));
			}
			else
			{	// Vector image (SVG or SVGZ):
				
				// Using svg-android lib:
				SVG svg = new SVGBuilder().readFromInputStream(new FileInputStream(file)).build();
				view.setImageDrawable(new SVGDrawable(svg));
				
				// Using AndroidSVG lib:
				//SVG svg = SVG.getFromInputStream(new FileInputStream(file));
		        //view.setImageDrawable(new PictureDrawable(svg.renderToPicture()));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load image from file", e);
		}
	}

}
