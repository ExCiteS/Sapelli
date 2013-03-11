/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.images;

import uk.ac.ucl.excites.collector.project.model.Project;
import android.graphics.BitmapFactory;
import android.widget.ImageView;

/**
 * @author mstevens
 *
 */
public class FileImage extends Image
{

	private String path;
	
	public FileImage(Project project, String imageLogicalPath)
	{
		this.path = project.getImagePath() + imageLogicalPath;
	}
	
	@Override
	protected void _setIn(ImageView imageView)
	{
		imageView.setImageBitmap(BitmapFactory.decodeFile(path));
	}

}
