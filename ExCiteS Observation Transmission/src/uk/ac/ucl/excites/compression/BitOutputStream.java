package uk.ac.ucl.excites.compression;


/*
    BitIO: A library for bit-oriented input/output.
    Copyright (C) 2009-2010 Jean-Francois Im

    This file is part of BitIO.

    BitIO is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BitIO is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with BitIO.  If not, see <http://www.gnu.org/licenses/>.
 */


import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

/**
 * A bit-oriented output stream.
 *
 * @author Jean-Francois Im
 */
public class BitOutputStream extends OutputStream {
    private OutputStream outputStream;
    private int currentByte = 0;
    private int currentBits = 0;

    /**
     * Constructs a BitOutputStream with a given output stream.
     *
     * @param outputStream The underlying output stream to write bits to.
     */
    public BitOutputStream(OutputStream outputStream) {
        this.outputStream = outputStream;
    }

    /**
     * Writes a single bit to the output stream.
     *
     * @param value The bit to write
     * @throws IOException If an underlying IOException occurs while writing to the stream
     */
    public void writeBit(final boolean value) throws IOException {
        currentBits++;
        currentByte <<= 1;

        if (value)
            currentByte += 1;

        if (currentBits == 8) {
            outputStream.write(currentByte);
            currentBits = 0;
            currentByte = 0;
        }
    }

    /**
     * Writes a number of zeroes to the output stream
     *
     * @param count The number of zeroes to write
     * @throws IOException If an underlying IOException occurs while writing to the stream
     */
    public void writeZeroes(final int count) throws IOException {
        if (count + currentBits < 8) {
            currentByte <<= count;
            currentBits += count;
        } else {
            // Fill the current byte with zeroes
            final int bitsWritten = 8 - currentBits;
            currentByte <<= bitsWritten;
            outputStream.write(currentByte);

            // Write zero bytes
            final int bitsRemaining = count - bitsWritten;
            final int bytesRemaining = bitsRemaining / 8;
            for (int i = 0; i < bytesRemaining; ++i) {
                outputStream.write(0);
            }

            // Set the current bit position to the number of remaining bits
            currentByte = 0;
            currentBits = bitsRemaining % 8;
        }
    }

    /**
     * Writes an unary-coded value to the output stream
     *
     * @param value The value to write to the output stream
     * @throws IOException If an underlying IOException occurs while writing to the stream
     */
    public void writeUnary(final int value) throws IOException {
        if (value + currentBits + 1 < 8) {
            currentByte <<= value + 1;
            currentByte += 1;
            currentBits += value + 1;
        } else {
            writeZeroes(value);
            writeBit(true);
        }
    }

    /**
     * Writes a certain number of bits to the output stream
     *
     * @param value   The value to write to the output stream
     * @param numBits The number of bits to be written to the output stream
     * @throws IOException If an underlying IOException occurs while writing to the stream
     */
    public void writeBinary(final int value, final int numBits) throws IOException {
        if (numBits + currentBits < 8) {
            currentByte <<= numBits;
            currentByte |= (value) & ((1 << numBits) - 1);
            currentBits += numBits;
        } else {
            // Write the top part
            final int bitsWritten = 8 - currentBits;
            currentByte <<= bitsWritten;
            currentByte |= (value >> (numBits - bitsWritten)) & ((1 << bitsWritten) - 1);
            outputStream.write(currentByte);

            // Write whole bytes
            final int bitsRemaining = numBits - bitsWritten;
            final int bytesToWrite = bitsRemaining / 8;
            final int bitOffset = bitsRemaining % 8;
            for (int i = bytesToWrite - 1; i >= 0; --i) {
                outputStream.write((value >> ((i << 3) + bitOffset)) & 0xFF);
            }

            // Write the bottom part
            currentByte = value & ((2 << bitOffset) - 1);
            currentBits = bitOffset;
        }
    }

    /**
     * Writes a Rice-coded value to the output stream
     *
     * @param value        The value to write
     * @param numFixedBits The number of bits used for the M parameter
     * @throws IOException If an underlying IOException occurs while writing to the stream
     */
    public void writeRice(int value, int numFixedBits) throws IOException {
        int m = 1 << numFixedBits;
        int q = (value - 1) / m;
        int r = value - (q << numFixedBits) - 1;

        writeUnary(q);
        writeBinary(r, numFixedBits);
    }

    /**
     * Closes the underlying output stream.
     *
     * @throws IOException If an IOException occurs while closing the stream
     */
    @Override
    public void close() throws IOException {
        if (currentBits > 0) {
            currentByte <<= 8 - currentBits;
            outputStream.write(currentByte);
            outputStream.close();
        }
    }

    @Override
    public void write(int value) throws IOException {
        if (currentBits == 0)
            outputStream.write(value);
        else {
            writeBinary(value, 8);
            // jfim: Code below is untested
            /*
            // Write the top part
            final int bitsWritten = 8 - currentBits;
            currentByte <<= bitsWritten;
            currentByte |= (value >> (8 - bitsWritten)) & ((2 << bitsWritten) - 1);
            outputStream.write(currentByte);

            // Write the bottom part
            final int bitsRemaining = 8 - bitsWritten;
            final int bitOffset = bitsRemaining % 8;
            currentByte = value & ((2 << bitOffset) - 1);
            currentBits = bitOffset;
            */
        }
    }

    @Override
    public void flush() throws IOException {
        outputStream.flush();
    }

	public void writeAsciiString(String name) throws IOException {
		OutputStreamWriter writer = new OutputStreamWriter(this, Charset.forName("ASCII"));
		
		writer.write(name);
		writer.write(0);
		writer.flush();
	}

    public int getTrashLength() {
        return (8 - currentBits) % 8;
    }
}
