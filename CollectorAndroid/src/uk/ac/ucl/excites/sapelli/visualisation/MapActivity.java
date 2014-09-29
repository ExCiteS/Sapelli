package uk.ac.ucl.excites.sapelli.visualisation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectActivity;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import android.app.ActionBar.LayoutParams;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Environment;
import android.util.DisplayMetrics;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.esri.android.map.Callout;
import com.esri.android.map.CalloutStyle;
import com.esri.android.map.GraphicsLayer;
import com.esri.android.map.MapView;
import com.esri.android.map.ags.ArcGISLocalTiledLayer;
import com.esri.android.map.event.OnSingleTapListener;
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
	public static String IMG_PATH = "img_path";

	private MapView map;
	private ArcGISLocalTiledLayer localMap;
	ProjectStore projectStore;
	RecordStore recordStore;
	private GraphicsLayer gLayer;
	Callout callout;

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

			}
		});

		// Sets 'OnSingleTapListener' so that when single tap
		// happens, Callout would show 'SQMI' information associated
		// with tapped 'Graphic'
		map.setOnSingleTapListener(new OnSingleTapListener() {

			private static final long serialVersionUID = 1L;

			public void onSingleTap(float x, float y) {

				if (!map.isLoaded())
					return;
				int[] uids = gLayer.getGraphicIDs(x, y, 2);
				if (uids != null && uids.length > 0) {

					int targetId = uids[0];
					Graphic gr = gLayer.getGraphic(targetId);
					callout = map.getCallout();

					// Sets Callout style
					callout.setStyle(R.xml.popup);
					CalloutStyle style = callout.getStyle();
					DisplayMetrics metrics = getResources().getDisplayMetrics();
					style.setMaxHeight(metrics.heightPixels);
					style.setMaxWidth(metrics.widthPixels * 2);

					// Sets custom content view to Callout
					callout.setContent(loadView(gr));
					//					map.centerAt(new Point(x, y), true);
					callout.show(map.toMapPoint(new Point(x, y)));
				} else {
					if (callout != null && callout.isShowing()) {
						callout.hide();
					}
				}

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
		displayData(records, project);
	}

	@SuppressWarnings("serial")
	public void displayData(List<Record> records, Project project) {
		Random random = new Random();
		int colour = Color.rgb(random.nextInt(256), random.nextInt(256), random.nextInt(256));

		int counter = 0;
		for (Record record : records) {
			SimpleMarkerSymbol sms = new SimpleMarkerSymbol(colour, 10, STYLE.DIAMOND);
			//			Location location = (Location) record.getSchema().getColumn("Location", true).retrieveValue(record);
			//			Point pnt = new Point(location.getLongitude(), location.getLatitude());

			Map<String, Object> imgPaths = new HashMap<String, Object>();

			Point pnt = null;
			for (Form form : project.getForms()) {
				for (Field field : form.getFields()) {
					if (field instanceof ChoiceField && !field.isNoColumn()) {

						ChoiceField cf = (ChoiceField) field;
						if (cf.getColumn().retrieveValue(record) != null) {
							String relPath = cf.getDictionary().lookupItem(cf.getColumn().retrieveValue(record).intValue()).getImageRelativePath();
							imgPaths.put(IMG_PATH + counter, project.getImageFolderPath() + relPath);
							counter++;
						}
					}

					if (field instanceof LocationField) {
						LocationField lf = (LocationField) field;
						pnt = new Point(lf.getColumn().retrieveValue(record).getLongitude(), lf.getColumn().retrieveValue(record).getLatitude());
					}
				}
			}

			Graphic graphic = new Graphic(GeometryEngine.project(pnt, SpatialReference.create(SpatialReference.WKID_WGS84), localMap.getSpatialReference()), sms, imgPaths);
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

	// Creates custom content view with 'Graphic' attributes
	private View loadView(Graphic graphic) {
		View view = LayoutInflater.from(MapActivity.this).inflate(R.layout.popup_layout, null);
		LinearLayout root = (LinearLayout) view.findViewById(R.id.popup_root);

		Set<String> keys = graphic.getAttributes().keySet();

		for (String key : keys) {
			if (key.startsWith(IMG_PATH)) {
				ImageView photo = new ImageView(MapActivity.this);
				photo.setImageBitmap(BitmapFactory.decodeFile(graphic.getAttributes().get(key).toString()));
				photo.setPadding(5, 0, 5, 0);
				LinearLayout.LayoutParams layoutParams = new LinearLayout.LayoutParams(150, 150);
				photo.setLayoutParams(layoutParams);
				root.addView(photo);
			}
		}

		return view;

	}
}
