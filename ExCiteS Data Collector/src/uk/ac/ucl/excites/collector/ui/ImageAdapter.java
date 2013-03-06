package uk.ac.ucl.excites.collector.ui;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.model.Audio;
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

public class ImageAdapter extends BaseAdapter
{

	static private int PADDING = 2; // pixels

	private Context context;
	private Project project;
	private int imageWidth;
	private int imageHeight;
	private List<String> custumIcons = new ArrayList<String>();
	private List<Integer> defaultIcons = new ArrayList<Integer>();

	private boolean invisible;
	private int invisiblePosition;

	public ImageAdapter(Context localContext, Project project, int imageWidth, int imageHeight)
	{
		this.context = localContext;
		this.project = project;
		this.imageWidth = imageWidth;
		this.imageHeight = imageHeight;
	}

	public int getCount()
	{
		if(defaultIcons.isEmpty())
			return custumIcons.size();
		else
			return defaultIcons.size();
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

			if(defaultIcons.isEmpty())
			{
				Bitmap bm = BitmapFactory.decodeFile(custumIcons.get(position).toString());
				imageView.setImageBitmap(bm);
				imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
			}
			else
			{
				imageView.setImageResource(defaultIcons.get(position));
				imageView.setScaleType(ImageView.ScaleType.CENTER);
			}
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

	public void IconsToDisplay(List<Choice> children)
	{
		for(Choice child : children)
		{
			custumIcons.add(project.getImagePath() + child.getImagePath());
		}
	}

	public void IconsToDisplay(Audio audio)
	{
		custumIcons.add(project.getImagePath() + audio.getRecordingImagePath());
		custumIcons.add(project.getImagePath() + audio.getStopImagePath());
	}

	public void buttonsToDisplay(boolean back, boolean cancel)
	{
		if(back)
			defaultIcons.add(R.drawable.back);
		if(cancel)
			defaultIcons.add(R.drawable.cancel);

	}

	public void audioIconsToDisplay()
	{

		defaultIcons.add(R.drawable.record);
		defaultIcons.add(R.drawable.stop);
	}

	public void setInvisible(int position)
	{
		invisiblePosition = position;
		invisible = true;
	}

}
