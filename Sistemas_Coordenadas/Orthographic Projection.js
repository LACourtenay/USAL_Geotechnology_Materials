
var chart = am4core.create("Globe", am4maps.MapChart);

chart.geodata = am4geodata_worldHigh;
chart.projection = new am4maps.projections.Orthographic();

var polygonSeries = chart.series.push(
    new am4maps.MapPolygonSeries()
);

polygonSeries.useGeodata = true;

var polygonTemplate = polygonSeries.mapPolygons.template;
polygonTemplate.fill = am4core.color("#A9A9A9");

var hs = polygonTemplate.states.create("hover");
hs.properties.fill = am4core.color("red");

var grid = chart.series.push(new am4maps.GraticuleSeries());
grid.toBack();

chart.panBehavior = "rotateLongLat";
chart.deltaLongitude = 4.6;