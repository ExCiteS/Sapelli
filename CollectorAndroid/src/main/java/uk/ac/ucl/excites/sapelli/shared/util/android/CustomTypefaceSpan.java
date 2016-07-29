/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.graphics.Paint;
import android.graphics.Typeface;
import android.text.TextPaint;
import android.text.style.MetricAffectingSpan;

/**
 * @author mstevens
 *
 * @see http://stackoverflow.com/a/17961854/1084488
 */
public class CustomTypefaceSpan extends MetricAffectingSpan
{
	
	private final Typeface typeface;

	public CustomTypefaceSpan(final Typeface typeface)
	{
		this.typeface = typeface;
	}

	@Override
	public void updateDrawState(final TextPaint drawState)
	{
		apply(drawState);
	}

	@Override
	public void updateMeasureState(final TextPaint paint)
	{
		apply(paint);
	}

	private void apply(final Paint paint)
	{
		final Typeface oldTypeface = paint.getTypeface();
		final int oldStyle = oldTypeface != null ? oldTypeface.getStyle() : 0;
		final int fakeStyle = oldStyle & ~typeface.getStyle();

		if((fakeStyle & Typeface.BOLD) != 0)
			paint.setFakeBoldText(true);
		if((fakeStyle & Typeface.ITALIC) != 0)
			paint.setTextSkewX(-0.25f);

		paint.setTypeface(typeface);
	}
	
}