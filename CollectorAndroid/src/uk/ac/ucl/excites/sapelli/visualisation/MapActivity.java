package uk.ac.ucl.excites.sapelli.visualisation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectActivity;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Environment;

import com.esri.android.map.GraphicsLayer;
import com.esri.android.map.MapView;
import com.esri.android.map.ags.ArcGISLocalTiledLayer;
import com.esri.android.map.event.OnStatusChangedListener;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.SpatialReference;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol.STYLE;

/**
 * @author Julia
 * 
 */
public class MapActivity extends ProjectActivity {

	// statics
	public static String BASEMAP = "/Download/London.tpk";
	public static String PROJECTHASH = "projectHash";

	private MapView map;
	private ArcGISLocalTiledLayer localMap;
	ProjectStore projectStore;
	RecordStore recordStore;
	private GraphicsLayer gLayer;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_map);

		try {
			projectStore = app.getProjectStore(this);
			recordStore = app.getRecordStore(this);
		} catch (Exception e) {
			showErrorDialog(getString(R.string.projectStorageAccessFail, ExceptionHelpers.getMessageAndCause(e)), true);
			return;
		}

		map = (MapView) findViewById(R.id.map);
		localMap = new ArcGISLocalTiledLayer(Environment.getExternalStorageDirectory().getPath() + BASEMAP);
		map.addLayer(localMap);
		//		map.addLayer(new ArcGISTiledMapServiceLayer("" + "http://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer"));

		// when map is initialised
		map.setOnStatusChangedListener(new OnStatusChangedListener() {
			private static final long serialVersionUID = 1L;

			// Once map is loaded set scale
			@Override
			public void onStatusChanged(Object source, STATUS status) {
				if (source == map && status == STATUS.INITIALIZED) {

					Bundle extras = getIntent().getExtras();
					if (extras != null) {
						long hash = extras.getLong(PROJECTHASH);
						gLayer = new GraphicsLayer();
						displayPerProject(projectStore.retrieveProject(hash));
					} else {
						gLayer = new GraphicsLayer();
						displayAllData();
					}
				}
				//				if (source == gLayer && status == STATUS.LAYER_LOADED) {
				//					Envelope point = new Envelope();
				//					Envelope allPoints = new Envelope();
				//					for (int i : gLayer.getGraphicIDs()) {
				//						Point p = (Point) gLayer.getGraphic(i).getGeometry();
				//						p.queryEnvelope(point);
				//						allPoints.merge(point);
				//					}
				//					map.setExtent(allPoints, 10, false);
				//				}
			}
		});
	}

	/**
	 * Display all data that is currently in the recordStore
	 */
	public void displayAllData() {

		List<Project> projects = projectStore.retrieveProjects();
		for (Project project : projects) {
			displayPerProject(project);
		}
	}

	public void displayPerProject(Project project) {

		List<Schema> schemata = new ArrayList<Schema>();

		for (Form f : project.getForms())
			if (f.isProducesRecords())
				schemata.add(f.getSchema());

		List<Record> records = recordStore.retrieveRecords(new RecordsQuery(schemata));
		displayData(records);
	}

	@SuppressWarnings("serial")
	public void displayData(List<Record> records) {
		Random random = new Random();
		int colour = Color.rgb(random.nextInt(256), random.nextInt(256), random.nextInt(256));

		for (Record record : records) {
			SimpleMarkerSymbol sms = new SimpleMarkerSymbol(colour, 10, STYLE.DIAMOND);
			Location location = (Location) record.getSchema().getColumn("Location", true).retrieveValue(record);
			Point pnt = new Point(location.getLongitude(), location.getLatitude());
			Graphic graphic = new Graphic(GeometryEngine.project(pnt, SpatialReference.create(SpatialReference.WKID_WGS84), localMap.getSpatialReference()), sms);
			gLayer.addGraphic(graphic);
		}
		map.addLayer(gLayer);
		gLayer.setOnStatusChangedListener(new OnStatusChangedListener() {
			@Override
			public void onStatusChanged(Object source, STATUS status) {
				if (source == gLayer && status == STATUS.INITIALIZED) {
					Envelope point = new Envelope();
					Envelope allPoints = new Envelope();
					for (int i : gLayer.getGraphicIDs()) {
						Point p = (Point) gLayer.getGraphic(i).getGeometry();
						p.queryEnvelope(point);
						allPoints.merge(point);
					}
					map.setExtent(allPoints, 100);

				}
			}
		});

	}

}
