/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.R;
import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

public class AudioItem extends Item implements FileItem
{

	private File file;

	public AudioItem(File file)
	{
		this(null, file);
	}

	public AudioItem(Integer id, File file)
	{
		super(id);
		this.file = file;
	}

	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		ImageView image = new ImageView(context);
		// TODO allow for custom
		SVG audioSvg = new SVGBuilder().readFromResource(context.getResources(), R.drawable.audio_item_svg).build();
		image.setScaleType(ScaleType.FIT_CENTER);
		image.setImageDrawable(new SVGDrawable(audioSvg));
		return image;
	}

	@Override
	public File getFile()
	{
		return file;
	}
}
