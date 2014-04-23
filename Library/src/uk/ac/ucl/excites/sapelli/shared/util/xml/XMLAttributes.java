/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.util.xml;

import org.xml.sax.Attributes;

import uk.ac.ucl.excites.sapelli.shared.util.Parameters;

/**
 * @author mstevens
 *
 */
public class XMLAttributes extends Parameters
{

	private Attributes attributes;
	
	/**
	 * @param attributes
	 */
	public XMLAttributes(Attributes attributes)
	{
		this.attributes = attributes;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.util.Parameters#getValue(java.lang.String)
	 */
	@Override
	public String getValue(String param)
	{
		return attributes.getValue(param);
	}

	@Override
	public boolean contains(String param)
	{
		return attributes.getIndex(param) != -1;
	}

	/**
	 * @return the attributes
	 */
	public Attributes getAttributes()
	{
		return attributes;
	}
	
}
