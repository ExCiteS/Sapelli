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

package uk.ac.ucl.excites.sapelli.shared.io.text;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PushbackInputStream;
import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * <p>
 * The <code>UnicodeBOMInputStream</code> class wraps any {@link InputStream} and detects the
 * presence of any Unicode BOM (Byte Order Mark) at its beginning (see {@link UnicodeBOM}).</p>
 * <p>
 * Use the {@link #getBOM()} method to know whether and which BOM has been detected.</p>
 * <p>
 * Use the {@link #skipBOM()} method to remove the detected BOM from the wrapped InputStream.</p>
 * <p>
 * Use the {@link #getReader()} method(s) to get an {@link InputStreamReader} instance to read from
 * the wrapped {@link InputStream}, configured with a {@link Charset} matching the BOM.</p>
 * <p>
 * This class is based on original work by Gregory Pakosz (@gpakosz), released under the terms of 
 * the Do What The Fuck You Want To Public License, Version 2 (WTFPL v2), which is arguably compatible
 * with the Apache License, Version 2. Gregory originally posted his implementation as a
 * <a href="http://stackoverflow.com/a/1835529/216063">Stack Overflow answer</a>, which is licensed
 * under CC-BY-SA 3.0.</p>

 * @author Gregory Pakosz, mstevens
 */
public class UnicodeBOMInputStream extends InputStream
{
	
	private final PushbackInputStream in;
	private final UnicodeBOM bom;
	private boolean skipped = false;
	
	/**
	 * Constructs a new <code>UnicodeBOMInputStream</code> that wraps the given {@link InputStream}.
	 * If there is a BOM it will *not* be automatically skipped.
	 *
	 * @param inputStream {@link InputStream}.
	 *
	 * @throws NullPointerException when <code>inputStream</code> is <code>null</code>.
	 * @throws IOException when an I/O error occurs upon reading from the given <code>InputStream</code> when trying to detect the Unicode BOM.
	 */
	public UnicodeBOMInputStream(final InputStream inputStream) throws NullPointerException, IOException
	{
		this(inputStream, false); // do not automatically skip the BOM
	}
	
	/**
	 * Constructs a new <code>UnicodeBOMInputStream</code> that wraps the given {@link InputStream}.
	 *
	 * @param inputStream {@link InputStream}.
	 * @param autoSkipBOM automatically skip the BOM bytes
	 *
	 * @throws NullPointerException when <code>inputStream</code> is <code>null</code>.
	 * @throws IOException when an I/O error occurs upon reading from the given <code>InputStream</code> when trying to detect the Unicode BOM.
	 */
	public UnicodeBOMInputStream(final InputStream inputStream, final boolean autoSkipBOM) throws NullPointerException, IOException
	{
		// Check inputStream:
		if(inputStream == null)
			throw new NullPointerException("Invalid InputStream: null is not allowed");
		// Wrap stream:
		this.in = new PushbackInputStream(inputStream, UnicodeBOM.GetMaxBOMLength());
		// Determine BOM:
		final byte[] bomBytes = new byte[UnicodeBOM.GetMaxBOMLength()];
		final int read = in.read(bomBytes);
		this.bom = UnicodeBOM.GetBOM(Arrays.copyOf(bomBytes, read));
		// Push back the read bytes:
		if(read > 0)
			in.unread(bomBytes, 0, read);
		// Skip the BOM (which may be shorter than the read bytes) if requested:
		if(autoSkipBOM)
			skipBOM();
	}

	/**
	 * Returns the {@link UnicodeBOM} that was detected in the wrapped {@link InputStream} object.
	 *
	 * @return a {@link UnicodeBOM} instance, never {@code null}. {@link UnicodeBOM#NO_BOM} indicates there was no (detectable/known) BOM.
	 */
	public final UnicodeBOM getBOM()
	{
		return bom;
	}
	
	/**
	 * Returns an {@link InputStreamReader} to read from the wrapped {@link InputStream}.
	 * The BOM will be used to set the {@link Charset} of the reader. If there was no (known/detectable) BOM the system default Charset will be used.
	 * If the BOM has not been skipped yet it will be before returning the reader.
	 * 
	 * @return an InputStreamReader
	 * @throws IOException when an I/O error occurs upon trying to skip the BOM from the wrapped <code>InputStream</code> object.
	 */
	public final synchronized InputStreamReader getReader() throws IOException
	{
		return getReader(Charset.defaultCharset());
	}
	
	/**
	 * Returns an {@link InputStreamReader} to read from the wrapped {@link InputStream}.
	 * The BOM will be used to set the {@link Charset} of the reader. If there was no (known/detectable) BOM the given fall-back Charset will be used.
	 * If the BOM has not been skipped yet it will be before returning the reader.
	 * 
	 * @return an InputStreamReader
	 * @throws IOException when an I/O error occurs upon trying to skip the BOM from the wrapped <code>InputStream</code> object.
	 */
	public final synchronized InputStreamReader getReader(Charset fallbackCharset) throws IOException
	{
		skipBOM();
		return new InputStreamReader(in, bom != UnicodeBOM.NO_BOM ? bom.getCharset() : fallbackCharset);
	}

	/**
	 * Skips the <code>BOM</code> that was found in the wrapped {@link InputStream} object.
	 *
	 * @return this <code>UnicodeBOMInputStream</code>.
	 *
	 * @throws IOException when an I/O error occurs upon trying to skip the BOM from the wrapped <code>InputStream</code> object.
	 */
	public final synchronized UnicodeBOMInputStream skipBOM() throws IOException
	{
		if(!skipped)
		{
			in.skip(bom.getLength());
			skipped = true;
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 */
	public int read() throws IOException
	{
		return in.read();
	}

	/**
	 * {@inheritDoc}
	 */
	public int read(final byte b[]) throws IOException, NullPointerException
	{
		return in.read(b, 0, b.length);
	}

	/**
	 * {@inheritDoc}
	 */
	public int read(final byte b[], final int off, final int len) throws IOException, NullPointerException
	{
		return in.read(b, off, len);
	}

	/**
	 * {@inheritDoc}
	 */
	public long skip(final long n) throws IOException
	{
		return in.skip(n);
	}

	/**
	 * {@inheritDoc}
	 */
	public int available() throws IOException
	{
		return in.available();
	}

	/**
	 * {@inheritDoc}
	 */
	public void close() throws IOException
	{
		in.close();
	}

	/**
	 * {@inheritDoc}
	 */
	public synchronized void mark(final int readlimit)
	{
		in.mark(readlimit);
	}

	/**
	 * {@inheritDoc}
	 */
	public synchronized void reset() throws IOException
	{
		in.reset();
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean markSupported()
	{
		return in.markSupported();
	}

}
