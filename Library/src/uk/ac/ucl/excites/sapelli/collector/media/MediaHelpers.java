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

package uk.ac.ucl.excites.sapelli.collector.media;

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
	private MediaHelpers()
	{
	}
	
	/**
	 * Pattern to recognise audio files by their extension.
	 * Based on supported audio files in Android: http://developer.android.com/guide/appendix/media-formats.html 
	 */
	static private final Pattern audioFilePattern = Pattern.compile("(.*/)*.+\\.(3gp|mp4|mp3|m4a|aac|ts|flac|mid|xmf|mxmf|rtttl|rtx|ota|imy|ogg|mkv|wav)$");
	
	/**
	 * Pattern to recognise image files by their extension.
	 */
	static private final Pattern imageFilePattern = Pattern.compile("(.*/)*.+\\.(png|jpg|gif|bmp|jpeg)$");
	
	/**
	 * Checks whether a filename has an audio file type extension
	 * 
	 * @param fileName
	 * @return
	 */
	static public boolean isAudioFileName(String fileName)
	{
		return audioFilePattern.matcher(fileName.toLowerCase()).matches();
	}

	/**
	 * Checks whether a filename has an image file type extension
	 * 
	 * @param fileName
	 * @return
	 */
	static public boolean isImageFileName(String fileName)
	{
		return imageFilePattern.matcher(fileName.toLowerCase()).matches();
	}

}
