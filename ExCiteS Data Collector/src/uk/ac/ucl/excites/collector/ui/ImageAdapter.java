package uk.ac.ucl.excites.collector.ui;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Choice;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.os.Environment;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.TableRow.LayoutParams;

public class ImageAdapter extends BaseAdapter
{
	
	static private int PADDING = 2; //pixels 
	
	private Context context;
	private List<String> selectedIcons = new ArrayList<String>();
	private int imageHeight;

	public ImageAdapter(Context localContext, int height)
	{
		this.context = localContext;
		this.imageHeight = height;
	}

	public int getCount()
	{
		return selectedIcons.size();
	}

	public Object getItem(int position)
	{
		return position;
	}

	public long getItemId(int position)
	{
		return position;
	}

	// create a new ImageView for each item referenced by the Adapter
	public View getView(int position, View convertView, ViewGroup parent)
	{
		ImageView imageView;
		if(convertView == null)
		{
			imageView = new ImageView(context);
			imageView.setBackgroundColor(Color.WHITE);
			Bitmap bm = BitmapFactory.decodeFile(selectedIcons.get(position).toString());
			imageView.setImageBitmap(bm);
			imageView.setLayoutParams(new GridView.LayoutParams(LayoutParams.WRAP_CONTENT, imageHeight));
			imageView.setPadding(PADDING, PADDING, PADDING, PADDING);
			imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
		}
		else
		{
			imageView = (ImageView) convertView;
		}

		return imageView;
	}

	public void IconsToDisplay(List<Choice> children)
	{
		for(Choice child : children)
		{
			selectedIcons.add(Environment.getExternalStorageDirectory() + "/ExCiteSImagePicker/Icons/" + child.getImagePath()); // path needs to be stored/passed as variable
		}
	}

	public void clearSelectedIcons()
	{
		selectedIcons.clear();
	}
}
