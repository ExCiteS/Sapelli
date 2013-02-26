package uk.ac.ucl.excites.collector.ui;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.Project;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.TableRow.LayoutParams;

public class ImageAdapter extends BaseAdapter
{

	static private int PADDING = 2; // pixels

	private Context context;
	private Project project;
	private int imageWidth;
	private int imageHeight;
	private List<String> selectedIcons = new ArrayList<String>();
	private List<Integer> buttonIDs = new ArrayList<Integer>();

	public ImageAdapter(Context localContext, Project project, int imageWidth, int imageHeight)
	{
		this.context = localContext;
		this.project = project;
		this.imageWidth = imageWidth;
		this.imageHeight = imageHeight;
	}
	
	public ImageAdapter(Context localContext, Project project, int imageHeight)
	{
		this.context = localContext;
		this.project = project;
		this.imageHeight = imageHeight;
	}

	public int getCount()
	{
		if(buttonIDs.isEmpty())
			return selectedIcons.size();
		else
			return buttonIDs.size();
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

			if(buttonIDs.isEmpty())
			{
				Bitmap bm = BitmapFactory.decodeFile(selectedIcons.get(position).toString());
				imageView.setImageBitmap(bm);
				imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
				imageView.setLayoutParams(new GridView.LayoutParams(imageWidth, imageHeight));
			}
			else
			{
				imageView.setImageResource(buttonIDs.get(position));
				imageView.setScaleType(ImageView.ScaleType.CENTER);
				imageView.setLayoutParams(new GridView.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));
			}
			imageView.setPadding(PADDING, PADDING, PADDING, PADDING);
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
			selectedIcons.add(project.getImagePath() + child.getImagePath()); // path needs to be stored/passed as variable
		}
	}

	public void buttonsToDisplay(boolean back, boolean cancel)
	{
		if (back)
			buttonIDs.add(R.drawable.back);
		if (cancel)
			buttonIDs.add(R.drawable.cancel);
	}

}
