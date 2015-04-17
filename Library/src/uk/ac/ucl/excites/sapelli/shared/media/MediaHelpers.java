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

package uk.ac.ucl.excites.sapelli.shared.media;

import java.util.regex.Pattern;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public final class MediaHelpers
{
	
	/**
	 * This class should never be instantiated 
	 */
	private MediaHelpers() {}
	
	/**
	 * Pattern to recognise audio files by their extension.
	 * 
	 * Based on supported audio/container file types in Android: http://developer.android.com/guide/appendix/media-formats.html
	 */
	static private final Pattern audioFilePattern = Pattern.compile("(.*/)*.+\\.(3gp|mp4|mp3|m4a|aac|ts|flac|mid|xmf|mxmf|rtttl|rtx|ota|imy|ogg|mkv|wav)$", Pattern.CASE_INSENSITIVE);
	
	/**
	 * Pattern to recognise raster image files by their extension.
	 */
	static private final Pattern rasterImageFilePattern = Pattern.compile("(.*/)*.+\\.(png|jpg|gif|bmp|jpeg)$", Pattern.CASE_INSENSITIVE);
	
	/**
	 * Pattern to recognise vector image files by their extension.
	 */
	static private final Pattern vectorImageFilePattern = Pattern.compile("(.*/)*.+\\.(svg|svgz)$", Pattern.CASE_INSENSITIVE);
	
	/**
	 * Checks whether a filename (or path) has an audio file type extension.
	 * 
	 * Recognises all Android-supported audio/container file types: http://developer.android.com/guide/appendix/media-formats.html
	 * 
	 * @param fileNameOrPath
	 * @return
	 */
	static public boolean isAudioFileName(String fileNameOrPath)
	{
		if(fileNameOrPath == null)
			return false;
		return audioFilePattern.matcher(fileNameOrPath).matches(); // "" will never match the pattern
	}

	/**
	 * Checks whether a filename (or path) has an image (raster or vector) file type extension (PNG, JPG/JPEG, GIF, BMP, SVG or SVGZ)
	 * 
	 * @param fileNameOrPath
	 * @return
	 */
	static public boolean isImageFileName(String fileNameOrPath)
	{
		return fileNameOrPath != null && (isRasterImageFileName(fileNameOrPath) || isVectorImageFileName(fileNameOrPath));
	}
	
	/**
	 * Checks whether a filename (or path) has an raster image file type extension (PNG, JPG/JPEG, GIF or BMP)
	 * 
	 * @param fileNameOrPath
	 * @return
	 */
	static public boolean isRasterImageFileName(String fileNameOrPath)
	{
		if(fileNameOrPath == null)
			return false;
		return rasterImageFilePattern.matcher(fileNameOrPath).matches(); // "" will never match the pattern
	}
	
	/**
	 * Checks whether a filename (or path) has an vector image file type extension (SVG or SVGZ)
	 * 
	 * @param fileNameOrPath
	 * @return
	 */
	static public boolean isVectorImageFileName(String fileNameOrPath)
	{
		if(fileNameOrPath == null)
			return false;
		return vectorImageFilePattern.matcher(fileNameOrPath).matches(); // "" will never match the pattern
	}

}
