package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EditTextField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;

public interface CollectorUI
{

	public FieldUI createChoiceUI(ChoiceField cf);

	public FieldUI createPhotoUI(PhotoField pf);

	public FieldUI createAudioUI(AudioField af);

	public FieldUI createLocationUI(LocationField lf);
	
	public FieldUI createLabelUI(LabelField lf);
	
	public FieldUI createButtonUI(ButtonField bf);

	public FieldUI createTextFieldUI(EditTextField tf);
	
	public FieldUI createCheckBoxFieldUI(CheckBoxField cbf);

	public FieldUI createButtonFieldUI(ButtonField bf);
	
	public FieldUI createMultiListUI(MultiListField mlf);
	
	public FieldUI createPageUI(Page page);

	public void setField(Field currentField);

}