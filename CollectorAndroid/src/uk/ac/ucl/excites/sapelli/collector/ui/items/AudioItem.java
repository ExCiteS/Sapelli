package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.R;
import android.content.Context;
import android.view.View;
import android.widget.ImageView;

import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

public class AudioItem extends Item implements FileItem {
	
	private File file;
	
	public AudioItem(File file) {
		this(null, file);
	}

	public AudioItem(Integer id, File file) {
	    super(id);
	    this.file = file;
    }

	@Override
	protected View createView(Context context, boolean recycleChildren) {
		ImageView image = new ImageView(context);
		SVG audioSvg = new SVGBuilder().readFromResource(context.getResources(), R.drawable.audio_item_svg).build();
		image.setImageDrawable(new SVGDrawable(audioSvg));
		return image;
	}

	@Override
    public File getFile() {
		return file;
    }

}
