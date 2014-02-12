package uk.ac.uk.excites.sapelli.server;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.io.ExCiteSFileLoader;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.util.FileHelpers;
import uk.ac.uk.excites.sapelli.server.db.DataAccessHelper;
import uk.ac.uk.excites.sapelli.server.util.Utilities;

/**
 * Servlet implementation class ProjectUpload
 * 
 * @author Michalis Vitos
 */
public class ProjectUpload extends HttpServlet
{
	public static final String EXCITES_FOLDER = "ExCiteS_Storage";
	public static final String PROJECTS_FOLDER = "Projects";

	private static final long serialVersionUID = 1L;
	private DataAccess dao;
	private String uploadMessage = "";

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ProjectUpload()
	{
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// Print the API version
		// PrintWriter out = response.getWriter();
		// ServletContext context = getServletContext();
		// Utilities.printServerVersion(context, out);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// Get context
		ServletContext context = getServletContext();

		// Get the database instance
		dao = DataAccessHelper.getInstance(request);

		// Upload Code
		File upFile = uploadFile(context, request);

		if(upFile != null)
		{
			// Extract and Parse project
			ExCiteSFileLoader loader = new ExCiteSFileLoader(getProjectsUploadFolderPath(context));
			Project loadedProject = null;
			try
			{
				loadedProject = loader.load(upFile);
			}
			catch(Exception e)
			{
				e.printStackTrace();
				uploadMessage = e.getLocalizedMessage();
			}

			// Save Project to DB
			try
			{
				dao.store(loadedProject);
				uploadMessage = "Successfully uploaded Project: " + loadedProject.getName();
			}
			catch(DuplicateException e)
			{
				e.printStackTrace();
				uploadMessage = e.getLocalizedMessage();
			}
		}

		// Print the list of Projects
		List<Project> projects = dao.retrieveProjects();
		String listOfProjects = "In total there are " + projects.size() + " projects. <br/><br/>";

		for(Project project : projects)
		{
			listOfProjects += "Project " + (projects.indexOf(project) + 1) + ": " + project.getName() + " in folder: <code>" + project.getDataFolderPath()
					+ "</code>";
			listOfProjects += "<br/>";
		}

		request.setAttribute("uploadMessage", uploadMessage);
		request.setAttribute("listOfProjects", listOfProjects);
		RequestDispatcher requestDispatcher = request.getRequestDispatcher("/project_upload.jsp");
		requestDispatcher.forward(request, response);
	}

	/**
	 * Method to upload the POSTed file to the Projects Upload Folder
	 * 
	 * @param context
	 * @param request
	 * @return
	 */
	private File uploadFile(ServletContext context, HttpServletRequest request)
	{
		File uploadedFile = null;

		try
		{
			List<FileItem> items = Utilities.castList(FileItem.class, new ServletFileUpload(new DiskFileItemFactory()).parseRequest(request));

			for(FileItem item : items)
			{
				if(item.isFormField())
				{
					// Does nothing for now
					// Process regular form field (input type="text|radio|checkbox|etc", select, etc).
					// String fieldname = item.getFieldName();
					// String fieldvalue = item.getString();
				}
				else
				{
					// Process form file field (input type="file").
					String filename = FilenameUtils.getName(item.getName());
					InputStream filecontent = item.getInputStream();

					uploadMessage += "filenamw: " + filename;

					// Upload the file to the specific file
					if(filename != null && !filename.isEmpty() && !filename.equals("No file chosen"))
					{
						uploadedFile = new File(getProjectsUploadFolderPath(context) + File.separator + filename);
						FileHelpers.copyFile(filecontent, uploadedFile);
					}
					else
					{
						uploadMessage = "You must select a file to upload";
					}
				}
			}
		}
		catch(FileUploadException e)
		{
			e.printStackTrace();
			uploadMessage = e.getLocalizedMessage();
		}
		catch(IOException e)
		{
			e.printStackTrace();
			uploadMessage = e.getLocalizedMessage();
		}

		return uploadedFile;
	}

	/**
	 * Returns the Uploads folder and creates the folder if does not exist
	 * 
	 * @param context
	 * @return
	 */
	public static File getExCiteSFolder(ServletContext context)
	{
		File folder = new File("/var/" + EXCITES_FOLDER + File.separator);
		FileHelpers.createFolder(folder);
		return folder;
	}

	/**
	 * Returns the Uploads folder path and creates the folder if does not exist
	 * 
	 * @param context
	 * @return
	 */
	public static String getExCiteSFolderPath(ServletContext context)
	{
		return getExCiteSFolder(context).getAbsolutePath() + File.separator;
	}

	/**
	 * Returns the Projects Uploads folder and creates the folder if does not exist
	 * 
	 * @param context
	 * @return
	 */
	public static File getProjectsUploadFolder(ServletContext context)
	{
		File folder = new File(getExCiteSFolderPath(context) + File.separator + PROJECTS_FOLDER + File.separator);
		FileHelpers.createFolder(folder);
		return folder;
	}

	/**
	 * Returns the Projects Uploads folder path and creates the folder if does not exist
	 * 
	 * @param context
	 * @return
	 */
	public static String getProjectsUploadFolderPath(ServletContext context)
	{
		return getProjectsUploadFolder(context).getAbsolutePath() + File.separator;
	}
}
