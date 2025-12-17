// Define bounding box
var bbox = ee.Geometry.Rectangle([-116.94713, 43.54980, -102.30368, 49.00377]);

// Assets
var zonalAssets = {
  'blm': ee.FeatureCollection('projects/ee-colinbrust/assets/blm'),
  'tribes': ee.FeatureCollection('projects/ee-colinbrust/assets/tribes'),
  'counties': ee.FeatureCollection('projects/ee-colinbrust/assets/counties'),
  'hucs': ee.FeatureCollection('projects/ee-colinbrust/assets/hucs')
};

var years = ee.List.sequence(2000, 2024);
var months = ee.List.sequence(1, 12);

// --- 1. DATASET PREPARATION (Monthly Timeseries) ---

var getMonthlySeries = function(collection, reducerType, scaleFactor, name) {
  var series = years.map(function(y) {
    return months.map(function(m) {
      var date = ee.Date.fromYMD(y, m, 1);
      var monthly = collection.filter(ee.Filter.calendarRange(y, y, 'year'))
                              .filter(ee.Filter.calendarRange(m, m, 'month'));

      var hasData = monthly.size().gt(0);
      var bandName = ee.String(name).cat('_').cat(date.format('YYYY_MM'));

      var result = ee.Image(ee.Algorithms.If({
        condition: hasData,
        trueCase: (reducerType === 'sum' ? monthly.sum() : monthly.mean()).multiply(scaleFactor),
        falseCase: ee.Image.constant(0).selfMask()
      }));

      // CRITICAL: Cast to double to ensure collection homogeneity
      return result.double()
                   .rename(name)
                   .set('system:time_start', date.millis());

    });
  }).flatten();
  return ee.ImageCollection.fromImages(series);
};

// Process all datasets
var etSeries = getMonthlySeries(ee.ImageCollection('MODIS/061/MOD16A2GF').select('ET'), 'sum', 0.1, 'ET');
var petSeries = getMonthlySeries(ee.ImageCollection('MODIS/061/MOD16A2GF').select('PET'), 'sum', 0.1, 'PET');
var gppSeries = getMonthlySeries(ee.ImageCollection('MODIS/061/MOD17A2HGF').select('Gpp'), 'sum', 0.0001, 'GPP');
var combinedVI = ee.ImageCollection('MODIS/061/MOD13A1').merge(ee.ImageCollection('MODIS/061/MYD13A1'));
var ndviSeries = getMonthlySeries(combinedVI.select('NDVI'), 'mean', 0.0001, 'NDVI');
var eviSeries = getMonthlySeries(combinedVI.select('EVI'), 'mean', 0.0001, 'EVI');

// Helper to stack and clean names
var stackAndClean = function(collections) {
  var stacked = ee.Image.cat(collections.map(function(c) { return c.toBands(); }));
  var bandNames = stacked.bandNames().map(function(n) {
    var parts = ee.String(n).split('_');
    return parts.slice(1).join('_'); // Removes '0_', '1_', etc.
  });
  return stacked.rename(bandNames);
};

var fullTimeStack = stackAndClean([etSeries, petSeries, gppSeries, ndviSeries, eviSeries]).clip(bbox);

// --- 2. CLIMATOLOGY PREPARATION (For TIF Exports) ---

var getAverages = function(monthlySeries, name) {
  var monthlyClim = months.map(function(m) {
    var monthName = ee.String(name).cat('_avg_month_').cat(ee.Number(m).format('%02d'));
    return monthlySeries.filter(ee.Filter.calendarRange(m, m, 'month'))
                        .mean()
                        .double() // Consistent type
                        .rename(monthName);
  });

  var annualAvg = monthlySeries.mean().double().rename(ee.String(name).cat('_annual_mean'));

  // Return as a list of images
  return ee.List(monthlyClim).add(annualAvg);
};

// Flatten all lists into one single list of images
var allTifImagesList = ee.List([])
  .cat(getAverages(etSeries, 'ET'))
  .cat(getAverages(petSeries, 'PET'))
  .cat(getAverages(gppSeries, 'GPP'))
  .cat(getAverages(ndviSeries, 'NDVI'))
  .cat(getAverages(eviSeries, 'EVI'));

// Convert list to a collection, then to a multi-band image
var tifStack = ee.ImageCollection.fromImages(allTifImagesList).toBands();

// Clean up the band names (remove the '0_', '1_' prefixes added by toBands)
var finalTifNames = tifStack.bandNames().map(function(n) {
  var parts = ee.String(n).split('_');
  return parts.slice(1).join('_');
});
tifStack = tifStack.rename(finalTifNames).clip(bbox);

// --- 3. EXPORTS ---

Export.image.toDrive({
  image: tifStack,
  description: 'MODIS_Averages_2000_2024',
  folder: 'Montana_MODIS',
  region: bbox,
  scale: 500,
  maxPixels: 1e13,
  crs: 'EPSG:4326'
});
// Zonal Exports
Object.keys(zonalAssets).forEach(function(key) {
  // Ensure the 'id' column is selected specifically
  var fc = zonalAssets[key].select(['id']);

  var zonalStats = fullTimeStack.reduceRegions({
    collection: fc,
    reducer: ee.Reducer.mean(),
    scale: 500,
    tileScale: 4
  });

  Export.table.toDrive({
    collection: zonalStats,
    description: 'Monthly_Timeseries_' + key,
    folder: 'Montana_MODIS_CSV',
    fileFormat: 'CSV'
  });
});

print('Tasks updated. CSV will include the "id" column and TIF uses homogeneous casting.');
