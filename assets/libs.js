// All dictionaries used in index.html. Abstracted out here to decrease clutter. 

var longNameMap = {
  'erc': 'Energy Release Component',
  'pr': 'Precipitation',
  'rmax': 'Max. Relative Humidity',
  'rmin': 'Min. Relative Humidity',
  'sph': 'Specific Humidity',
  'srad': 'Solar Radiation',
  // 'th': 'Wind Direction',
  'tmmn': 'Min. Air Temperature',
  'tmmx': 'Max. Air Temperature',
  'vpd': 'Vapor Pressure Deficit',
  'vs': 'Wind Speed'
};

var typeMap = {
  'mean': 'Mean',
  'mode': 'Mode', 
  'median': 'Median', 
  'variance': 'Variance',
  'alpha': 'Alpha',
  'beta': 'Beta',
  'quantiles': 'Quantiles'
};

var timeHash = {
  'Annual': 'annual',
  'January': 'jan',
  'February': 'feb',
  'March': 'mar',
  'April': 'apr',
  'May': 'may',
  'June': 'jun',
  'July': 'jul',
  'August': 'aug',
  'September': 'sep',
  'October': 'oct',
  'November': 'nov',
  'December': 'dec'
}

var colorMap = {
  'alpha': 'PuOr',
  'beta': 'YlOrRd',
  'pr': 'BrBG',
  'tmmn': 'Blues',
  'tmmx': 'Reds',
  'rmax': 'BrBG',
  'rmin': 'BrBG',
  'th': 'PuRd',
  'erc': 'PuRd',
  'vpd': 'PuRd',
  'vs': 'PuRd',
  'sph': 'PuOr',
  'srad': 'RdBu',
}

var legendMap = {
  'rmax': 'Relative Humidity (%)',
  'rmin': 'Relative Humidity (%)',
  'sph': 'Specific Humidity (kg/kg)',
  'th': 'Wind Direction (deg.)',
  'srad': 'Solar Radiation (W/m^2)',
  'vs': 'Wind Speed (m/s)',
  'erc': 'Energy Release Index',
  'vpd': 'Vapor Pressure Deficit (kPa)', 
  'pr': 'Precipitation (in)',
  'tmmx': 'Temperature (F)',
  'tmmn': 'Temperature (F)',
  'alpha': 'Alpha Parameter',
  'beta': 'Beta Parameter',
}

var labs = [
  "a) 1st Percentile", "b) 10th Percentile", "c) 20th Percentile", "d) 30th Percentile", "e) 40th Percentile",
  "f) 50th Percentile", "g) 60th Percentile", "h) 70th Percentile", "i) 80th Percentile", "j) 90th Percentile", "k) 99th Percentile" 
]

var update_variable_text = function(v) {
  switch (v) {

    case 'erc':
      return `
        Average daily energy release component. The amount of available energy that a fire could burn at a given area. 
      `
    case 'pr':
      return `
        Total precipitation for the given time period in inches. Precipitation is the combination of rainfall and snowfall. 
      `
    case 'rmax':
      return `
          Average daily maximum relative humidity (%). The amount of water vapor in the air relative to the how much water vapor the air can theoretically hold. 
          Relative humidity varies based on air temperature and pressure. 
        `
    case 'rmin':
      return `
        Average daily minimum relative humidity (%). The amount of water vapor in the air relative to the how much water vapor the air can theoretically hold. 
          Relative humidity varies based on air temperature and pressure.
      `
    case 'sph':
      return `
        Average daily specific humidity (kg/kg). The mass of water vapor in one kilogram of air. Unlike relative humidity, specific humidity does not change
        with air temperature and pressure. 
      `
    case 'srad':
      return `
        Average daily incoming shortwave solar radiation (W/m^2). 
      `
    case 'tmmn':
      return `
        Daily average mainimum temperature (deg F). 
      `
    case 'tmmx':
      return `
        Daily average maximum temperature (deg F).
      `
    case 'vpd':
      return `
        Average daily vapor pressure deficit (kPa). The difference between how much moisture is currently in the air versus how much moisture can theoretically 
        be held in the air.
      `
    case 'vs':
      return `
        The daily average wind speed (m/s).
      `
  }
}

var update_statistic_text = function(s) {
  switch (s) {

    case 'alpha':
      return `
        The alpha parameter of the gamma distribution dictates the shape of the distribution's curve.
      `
    case 'beta':
      return `
        The beta parameter of the gamma distribution dictates the rate at which the distribution's curve rises and falls. 
      `
    case 'mean':
      return `
          The mean is the average value of the selected variable for the 1991 - 2020 record. Because the mean is taken across all values, 
          large outliers can skew the value of the mean. 
        `
    case 'median':
      return `
            The median is the value that falls in the middle of all values for the 1991 - 2020 record. Unlike the mean, the median is not affected by 
            outliers. 
        `
    case 'mode':
      return `
          The mode is the value that is most likely to be sampled from a set of data. In the context of the gamma distribution, it is the value that is 
          probabalistically most likely to occur (check). 
        `
    case 'quantiles':
      return `
          Quantiles are a method of breaking data into equally likely groups. One of the most common uses of quantiles are the median and interquartile range (IQR), 
          which measure the center and spread of a set of data. Here, you can adjust the above slider to visualize different quantiles. 
        `
    case 'variance':
      return `
        The variance quantifies how much each individual data point varies relative to the mean of the distribution. 
      `
    }
}