package uk.ac.ucl.excites.sapelli.shared.util.android;

import java.lang.reflect.Method;

import android.util.Log;
import android.view.Menu;

/**
 * Helper methods to deal with menus.
 * 
 * @author mstevens
 */
public final class MenuHelpers
{

	private MenuHelpers() {}
	
	static private final Class<?> AndroidMenuBuilderClass;
	static
	{
		Class<?> amb = null;
		try
		{
			amb = Class.forName("com.android.internal.view.menu.MenuBuilder");
		}
		catch(ClassNotFoundException e)
		{
			e.printStackTrace(System.err);
		}
		AndroidMenuBuilderClass = amb;
	}
	
	static private final Class<?> AppCompatMenuBuilderClass = android.support.v7.internal.view.menu.MenuBuilder.class;
	
	static private final Class<?>[] MenuBuilderClasses = { AndroidMenuBuilderClass, AppCompatMenuBuilderClass };
	
	static private final Class<?> AndroidMenuItemImplClass;
	static
	{
		Class<?> amii = null;
		try
		{
			amii = Class.forName("com.android.internal.view.menu.MenuItemImpl");
		}
		catch(ClassNotFoundException e)
		{
			e.printStackTrace(System.err);
		}
		AndroidMenuItemImplClass = amii;
	}
	
	static private final Class<?> AppCompatMenuItemImplClass = android.support.v7.internal.view.menu.MenuItemImpl.class;
	
	static protected final Class<?>[] MenuItemImplClasses = { AndroidMenuItemImplClass, AppCompatMenuItemImplClass };
	
	/**
	 * Method which forces the displaying of action icons in overflow menus (which Android normally shows as text-only).
	 * 
	 * @param menu
	 * 
	 * @see Solution working everywhere, including on appcompat v22+ (code below is based on this): http://stackoverflow.com/a/30337653/1084488
	 * @see Solution for older versions: http://stackoverflow.com/a/22288914/1084488
	 */
	static public void forceMenuIcons(Menu menu)
	{
		if(menu == null)
			return;
		for(Class<?> MenuBuilderClass : MenuBuilderClasses)
			if(menu.getClass().equals(MenuBuilderClass))
			{
				try
				{
					final Method setOptionalIconsVisible = MenuBuilderClass.getDeclaredMethod("setOptionalIconsVisible", Boolean.TYPE);
					setOptionalIconsVisible.setAccessible(true);
					setOptionalIconsVisible.invoke(menu, true);
				}
				catch(Exception e)
				{
					Log.e(MenuHelpers.class.getSimpleName(), "Unable to force displaying of icons in overflow menu (class: " + menu.getClass().getName() + ")", e);
				}
				break;
			}
	}
	
	/**
	 * Doesn't work
	 * 
	 * @param item
	 * @return
	 */
//	static public boolean isMenuItemInOverflow(MenuItem item)
//	{
//		if(item == null)
//			return false;
//		for(Class<?> MenuItemImplClass : MenuItemImplClasses)
//			if(item.getClass().equals(MenuItemImplClass))
//			{
//				Log.d(MenuHelpers.class.getSimpleName(), "Trying to detect whether MenuItem \"" + item.getTitle() + "\" (class: " + item.getClass().getName() + ") is in the overflow menu of not...");
//			    try
//			    {
//			    	final Method isActionButton = MenuItemImplClass.getDeclaredMethod("isActionButton");
//			        isActionButton.setAccessible(true);
//			        return !(Boolean) isActionButton.invoke(item);
//			    }
//			    catch(Exception e)
//			    {
//			    	Log.e(MenuHelpers.class.getSimpleName(), "Unable to detect whether MenuItem \"" + item.getTitle() + "\" (class: " + item.getClass().getName() + ") is in the overflow menu of not.", e);
//			    	return false;
//			    }
//			}
//		return false;
//	}
	
}
