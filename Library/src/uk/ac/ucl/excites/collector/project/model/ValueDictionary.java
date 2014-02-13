/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author mstevens
 *
 */
public class ValueDictionary
{

	static public final int UNKNOWN_VALUE_CODE = -1;
	
	private Map<String, Integer> labelToCode;
	private List<String> codeToLabel;
	
	public ValueDictionary()
	{
		this.labelToCode = new HashMap<String, Integer>();
		this.codeToLabel = new ArrayList<String>();
	}
	
	public void addValue(String label)
	{
		if(label != null && !label.isEmpty() && !labelToCode.containsKey(label))
		{
			codeToLabel.add(label); //adds at the end of the list
			Integer code = Integer.valueOf(codeToLabel.size() - 1); //index of value in the list
			labelToCode.put(label, code);
		}
	}
	
	public int lookupCode(String label)
	{
		Integer code = labelToCode.get(label);
		return (code != null ? code : UNKNOWN_VALUE_CODE);
	}
	
	public String lookupLabel(int code)
	{
		return codeToLabel.get(code);
	}
	
	public int size()
	{
		return labelToCode.keySet().size();
	}
	
}
