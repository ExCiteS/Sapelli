package uk.ac.ucl.excites.sapelli.packager.io;

import java.nio.file.Path;

/**
 * Created by Michalis on 28/06/2017.
 */
public interface ProjectListener
{
	void onFileChanged(Path path);
}
