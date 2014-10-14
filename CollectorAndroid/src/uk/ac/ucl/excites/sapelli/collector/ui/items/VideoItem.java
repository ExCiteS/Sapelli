package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.io.File;

import android.content.Context;
import android.graphics.Bitmap;
import android.media.ThumbnailUtils;
import android.provider.MediaStore;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

public class VideoItem extends Item implements FileItem {
	
	private File file;
	
	public VideoItem(File file) {
		this(null, file);
	}

	public VideoItem(Integer id, File file) {
	    super(id);
	    this.file = file;
    }

	@Override
	protected View createView(Context context, boolean recycleChildren) {
		ImageView image = new ImageView(context);
		// create thumbnail from video file:
		Bitmap thumbnail = ThumbnailUtils.createVideoThumbnail(file.getAbsolutePath(),MediaStore.Images.Thumbnails.MINI_KIND);
		image.setScaleType(ScaleType.FIT_CENTER);
		image.setImageBitmap(thumbnail);
		return image;
	}

	@Override
    public File getFile() {
		return file;
    }

}
