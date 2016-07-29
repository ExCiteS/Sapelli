package uk.ac.ucl.excites.sapelli.shared.io.text;

import java.io.IOException;

import static uk.ac.ucl.excites.sapelli.shared.io.text.CharsetHelpers.CMMBPC_PROPERTIES_FILE_NAME;
import static uk.ac.ucl.excites.sapelli.shared.io.text.CharsetHelpers.PROPERTIES_FILE_EXTENSION;

/**
 * @author mstevens
 */
public class UpdateCMMBPCProperties
{

    /**
     * @param args 0 = inputFolderPath; 1 = outputFolderPath
     * @throws IOException
     */
    public static void main(String[] args) throws IOException
    {
        boolean updated = CharsetHelpers.GeneratePropertiesFile(args[0], args[1], true);
        System.out.println(CMMBPC_PROPERTIES_FILE_NAME + "." + PROPERTIES_FILE_EXTENSION + " " + (updated ? "updated" : "unchanged") + ".");
    }

}
