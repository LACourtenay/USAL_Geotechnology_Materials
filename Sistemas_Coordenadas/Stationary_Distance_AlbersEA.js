
var chart = am4core.create("AlbersEA_Distance", am4maps.MapChart);

chart.geodata = am4geodata_worldHigh;
chart.projection = new am4maps.projections.Albers();

var polygonSeries = chart.series.push(
    new am4maps.MapPolygonSeries()
);

polygonSeries.useGeodata = true;

var polygonTemplate = polygonSeries.mapPolygons.template;
polygonTemplate.fill = am4core.color("#A9A9A9");

var hs = polygonTemplate.states.create("hover");
hs.properties.fill = am4core.color("red");

var lineSeries1 = chart.series.push(new am4maps.MapLineSeries());
lineSeries1.data = [{
    "multiGeoLine": [
        [
            {"latitude": 51.47, "longitude": 0.453},
            {"latitude": 48.856614, "longitude": 2.352222},
            {"latitude": 40.712775, "longitude": -74.005973}
        ]
    ]
}];
lineSeries1.mapLines.template.line.strokeWidth = 1.5;
lineSeries1.mapLines.template.line.strokeOpacity = 1;

polygonSeries.data = [{
    "id" : "GL",
    "name": "Greenland",
    "fill": am4core.color("#F05C5C")
}];
polygonTemplate.propertyFields.fill = "fill";

var grid = chart.series.push(new am4maps.GraticuleSeries());
grid.toBack();

chart.deltaLongitude = 4.6;

