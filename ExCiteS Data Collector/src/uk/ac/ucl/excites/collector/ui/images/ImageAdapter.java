package uk.ac.ucl.excites.collector.ui.images;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

/**
 * @author Julia, mstevens
 *
 */
public class ImageAdapter extends BaseAdapter
{

	static private final int PADDING = 2; // pixels
	static private final int DEFAULT_BACKGROUND_COLOR = Color.WHITE;
	static private final int DEFAULT_IMAGE_HEIGHT = 140;
	static private final int DEFAULT_IMAGE_WIDTH = 140;
	static private final ScaleType DEFAULT_SCALE_TYPE = ScaleType.CENTER_INSIDE;
	
	private Context context;
	private int imageWidth;
	private int imageHeight;
	private ScaleType scaleType;
	private int backgroundColor;
	private List<Image> images;
	
	public ImageAdapter(Context localContext)
	{
		this.context = localContext;
		this.imageHeight = DEFAULT_IMAGE_HEIGHT;
		this.imageWidth = DEFAULT_IMAGE_WIDTH;
		this.scaleType = DEFAULT_SCALE_TYPE;
		this.backgroundColor = DEFAULT_BACKGROUND_COLOR;
		this.images = new ArrayList<Image>();
	}
	
	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(int backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}
	
	/**
	 * @param imageHeight the imageHeight to set
	 */
	public void setImageHeight(int imageHeight)
	{
		this.imageHeight = imageHeight;
	}

	/**
	 * @param imageWidth the imageWidth to set
	 */
	public void setImageWidth(int imageWidth)
	{
		this.imageWidth = imageWidth;
	}
	
	/**
	 * @param scaleType the scaleType to set
	 */
	public void setScaleType(ScaleType scaleType)
	{
		this.scaleType = scaleType;
	}

	public void addImage(Image image)
	{
		images.add(image);
	}
	
	public void clear()
	{
		images.clear();
	}

	public int getCount()
	{
		return images.size();
	}

	public Image getItem(int position)
	{
		return images.get(position);
	}

	public long getItemId(int position)
	{
		return position;
	}

	/**
	 * Create a new ImageView for each item referenced by the Adapter
	 */
	public View getView(int position, View convertView, ViewGroup parent)
	{
		ImageView imageView;
		if(convertView == null)
		{
			imageView = new ImageView(context);
			imageView.setBackgroundColor(backgroundColor);

			Image img = images.get(position);
			img.setIn(imageView); //sets the image (if it is not invisible)
			imageView.setScaleType(scaleType);
			imageView.setLayoutParams(new GridView.LayoutParams(imageWidth, imageHeight));
			imageView.setPadding(PADDING, PADDING, PADDING, PADDING);
		}
		else
		{
			imageView = (ImageView) convertView;
		}
		return imageView;
	}

	public void makeInvisible(int position)
	{
		getItem(position).setVisibility(false);
	}
	
	public void makeVisible(int position)
	{
		getItem(position).setVisibility(true);
	}

}
