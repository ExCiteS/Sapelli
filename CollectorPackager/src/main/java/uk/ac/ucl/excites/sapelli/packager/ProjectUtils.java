package uk.ac.ucl.excites.sapelli.packager;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * Various Utils for Sapelli Projects
 * <p>
 * Created by Michalis on 26/05/2017.
 */
public class ProjectUtils
{
	public static String printProjectInfo(Project project)
	{
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("Project info:");
		stringBuilder.append("\n");
		stringBuilder.append(" - id: ").append(project.getID());
		stringBuilder.append("\n");
		stringBuilder.append(" - fingerprint: ").append(project.getFingerPrint());
		stringBuilder.append("\n");
		stringBuilder.append(" - name: ").append(project.getName());
		stringBuilder.append("\n");
		stringBuilder.append(" - variant: ").append(project.getVariant());
		stringBuilder.append("\n");
		stringBuilder.append(" - version: ").append(project.getVersion());
		stringBuilder.append("\n");
		stringBuilder.append(" - display name: ").append(project.toString(false));
		stringBuilder.append("\n");
		stringBuilder.append(" - model id: ").append(project.getModel().id);
		stringBuilder.append("\n");
		stringBuilder.append(" - Forms:");
		stringBuilder.append("\n");
		int f = 0;
		for(Form form : project.getForms())
		{
			stringBuilder.append("    * Form ").append(++f).append(" info:");
			stringBuilder.append("\n");
			stringBuilder.append("       - id: ").append(form.id);
			stringBuilder.append("\n");
			stringBuilder.append("       - producesData: ").append(form.isProducesRecords());
			stringBuilder.append("\n");
			stringBuilder.append("       - model schema number: ").append((form.isProducesRecords() ? form.getSchema().getModelSchemaNumber() : "n/a"));
			stringBuilder.append("\n");
		}

		return stringBuilder.toString();
	}
}
