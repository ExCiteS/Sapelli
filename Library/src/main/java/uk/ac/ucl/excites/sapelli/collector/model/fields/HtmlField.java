/**
 * Sapelli data collection platform: http://sapelli.org
 * <p>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.HtmlUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * A Field class representing an html page
 *
 * @author Michalis Vitos
 */
public class HtmlField extends Field
{

	static public final String ID_PREFIX = "html";

	//Defaults:
	static public final boolean DEFAULT_EXTERNAL_LINKS = false;

	// Dynamics
	private String url;
	private boolean opensLinksExternally = DEFAULT_EXTERNAL_LINKS;

	/**
	 * @param form
	 * @param id      the id of the field, may be null (but not recommended)
	 * @param caption the caption of the field, may be null (in which case there will be no text label above the textbox)
	 */
	public HtmlField(Form form, String id, String caption)
	{
		super(form,
				GetID(id, form, ID_PREFIX, caption),
				caption);
	}

	@Override
	protected Column<?> createColumn(String name)
	{
		// An Html field is always noColumn, so that it does not store any info
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterHtmlField(this, arguments, withPage);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> HtmlUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createHtmlUI(this);
	}

	/**
	 * @return the URL of the HtmlField
	 */
	public String getUrl()
	{
		return url;
	}

	/**
	 * @param url - the URL of the HtmlField
	 */
	public void setUrl(String url)
	{
		this.url = url;
	}

	/**
	 * @return get whether links open externally
	 */
	public boolean opensLinksExternally()
	{
		return opensLinksExternally;
	}

	/**
	 * @param opensLinksExternally - set whether links open externally
	 */
	public void setOpensLinksExternally(boolean opensLinksExternally)
	{
		this.opensLinksExternally = opensLinksExternally;
	}
}
