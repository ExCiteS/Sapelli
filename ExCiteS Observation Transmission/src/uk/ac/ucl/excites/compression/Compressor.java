package uk.ac.ucl.excites.compression;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.TreeMap;

public class Compressor {

	public static byte[] compress(byte[] value) throws IOException {
		ByteArrayOutputStream rawOutputStream = new ByteArrayOutputStream();
		BitOutputStream bitOutputStream = new BitOutputStream(rawOutputStream);

		CharacterTree tree = new CharacterTree(getFrequenceTable(value));
		tree.serializeTo(bitOutputStream);

		tree.write(new ByteArrayInputStream(value), bitOutputStream);

		byte[] ouput = rawOutputStream.toByteArray();
		return ouput;
	}

	public static byte[] DeCompress(byte[] value) throws IOException {

		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		ByteArrayInputStream input = new ByteArrayInputStream(value);
		BitInputStream bitInputStream = new BitInputStream(input);

		CharacterTree tree = new CharacterTree(bitInputStream);
		ByteArrayOutputStream output = tree.read(bitInputStream, outputStream);
		byte[] decompressed = output.toByteArray();
		return decompressed;
	}

	private static Map<Integer, Integer> getFrequenceTable(byte[] value) {
		Map<Integer, Integer> frequenceTable = new TreeMap<Integer, Integer>();

		for (byte letter : value) {
			int letterValue = letter & 0x007F;

			if (letter < 0) {
				letterValue = letterValue | 0x0080;
			}

			if (frequenceTable.containsKey(letterValue)) {
				frequenceTable.put(letterValue,
						frequenceTable.get(letterValue) + 1);
			} else {
				frequenceTable.put(letterValue, 1);
			}
		}

		return frequenceTable;
	}

}
