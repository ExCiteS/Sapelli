package uk.ac.ucl.excites.sapelli.collector.control;

import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.lists.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.project.ui.ControlsState;

public abstract class Controller
{
	
	/**
	 * @param cf  the ChoiceField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterChoiceField(ChoiceField cf);
	
	/**
	 * @param af  the MediaField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterMediaField(MediaField af);
	
	/**
	 * @param pf  the PhotoField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterPhotoField(PhotoField pf);
	
	/**
	 * @param lf  the LocationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterLocationField(LocationField lf);
	
	/**
	 * @param of  the OrientationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterOrientationField(OrientationField of);
	
	/**
	 * @param tf  the EditTextField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterTextField(EditTextField tf);
	
	/**
	 * @param tf  the CheckBoxField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterCheckBoxField(CheckBoxField cbf);
	
	/**
	 * @param mlf  the MultiListField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterMultiListField(MultiListField mlf);

	/**
	 * @param bf  the ButtonField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterButtonField(ButtonField bf);
	
	/**
	 * @param page	the Page
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterPage(Page page);
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterEndField(EndField ef);
	
	/**
	 * @return the current ButtonState
	 */
	public abstract ControlsState getControlsState();
	
}
