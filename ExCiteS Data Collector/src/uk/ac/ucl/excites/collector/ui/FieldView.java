package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.util.Cancelable;
import android.view.View;

public interface FieldView extends Cancelable
{

	public void initialise(ProjectController controller, Field field);
	
	/**
	 * Should return the android.view.View to display, this is most likely the object itself
	 * (we use this trick for lack of multiple inheritance)
	 * 
	 * @return the view
	 */
	public View getView();
	
}
