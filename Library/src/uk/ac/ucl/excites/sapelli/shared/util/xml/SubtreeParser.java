/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.util.xml;

/**
 * @author mstevens
 *
 */
public abstract class SubtreeParser extends Handler
{

	protected final Handler owner;
	private final String rootElementQName;
	
	public SubtreeParser(Handler owner, String rootElementQName)
	{
		this.owner = owner;
		this.rootElementQName = rootElementQName;
	}
	
	@Override
	public void startDocument()
	{
		throw new UnsupportedOperationException("XMLSubtreeParsers are not meant to parse entire documents");
	}
	
	@Override
	public void endDocument()
	{
		throw new UnsupportedOperationException("XMLSubtreeParsers are not meant to parse entire documents");
	}
	
	protected void activate()
	{
		owner.activateSubtreeParser(this);
	}
	
	public void deactivate()
	{
		owner.deactivateSubtreeParser(this);
		reset();
	}
	
	public boolean isActive()
	{
		return owner.getActiveSubtreeParser() == this;
	}
	
	public final String getRootElementQName()
	{
		return rootElementQName;
	}
	
	protected abstract void reset();
	
	protected abstract boolean isSingleUse();
	
}
