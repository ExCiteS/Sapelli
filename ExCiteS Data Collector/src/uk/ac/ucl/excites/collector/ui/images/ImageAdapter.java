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

/**
 * @author Julia, mstevens
 *
 */
public class ImageAdapter extends BaseAdapter
{

	static private int PADDING = 2; // pixels

	private Context context;
	private int imageWidth;
	private int imageHeight;
	
	private List<Image> images;

	private boolean invisible;
	private int invisiblePosition;

	public ImageAdapter(Context localContext, int imageWidth, int imageHeight)
	{
		this.context = localContext;
		this.imageWidth = imageWidth;
		this.imageHeight = imageHeight;
		this.images = new ArrayList<Image>();
	}
	
	public void addImage(Image image)
	{
		images.add(image);
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
			imageView.setBackgroundColor(Color.WHITE);

			images.get(position).setIn(imageView); //set the image
			imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
			imageView.setLayoutParams(new GridView.LayoutParams(imageWidth, imageHeight));
			imageView.setPadding(PADDING, PADDING, PADDING, PADDING);
			
			if(invisible && position == invisiblePosition)
				imageView.setVisibility(View.INVISIBLE);
		}
		else
		{
			imageView = (ImageView) convertView;
		}
		return imageView;
	}

	public void setInvisible(int position)
	{
		invisiblePosition = position;
		invisible = true;
	}

}
