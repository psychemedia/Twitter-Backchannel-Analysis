// Google Apps Script Snippet for POST handling data from R used in generate-sna-stats-push-back-to-spreadsheet.R
// For more information on usage see http://mashe.hawksey.info/2012/01/tags-r/
// Compatibility: 3.1+

function doPost(e){
  // SCRIPT WHICH HANDLES DATA POSTED FROM R
  // To use complete steps 1 -3 from TAGS - Advanced menu and File > Publish to the web..
  // From this window select Share > Publish as service.. allowing anyone to invloke annonymously
  // Modify and run generate-sna-stats-push-back-to-spreadsheet.R
  var secret ="FzWGlpUkt1Tmc"; //must match secret in R script
  if (e.parameter.secret == secret){
    var ss = SpreadsheetApp.openById(ScriptProperties.getProperty('active')); 
    var sheet = ss.getSheetByName("Vertices"); // read the existing data from Vertices sheet
    var data = {}
    var datagrid = Utilities.jsonParse(e.parameter.datagrid); // handling datagrid object made in R
    var datagridlabels = Utilities.jsonParse(e.parameter.datagridlabels); // list of screen_names that match datagrid made in R
    for (i in datagrid){
      datagrid[i].screen_name = datagridlabels[i]; 
      data[datagridlabels[i]]=datagrid[i]; // index objects with screennames
    }
    var toptens = Utilities.jsonParse(e.parameter.toptens); // collect toptens list made in R
    var labels = Utilities.jsonParse(e.parameter.toptenslabels); //list of names of things calculated in R
    var exdata = getRowsData(sheet); // read the existing data from Vertices sheet
    for (i in exdata){ // add new data to existing data
      if (data[exdata[i].screen_name] != undefined){
        for (j in labels){
          exdata[i][labels[j]] = data[exdata[i].screen_name][labels[j]];
        }
      }
    }
    setRowsData(sheet,exdata); // write individuals stats back to sheet 

    var sheet = ss.getSheetByName("SNA Metrics"); // add the top ten's to SNA Metrics sheet 
    var topsData = chunk(toptens,10);
    // probably could have found better way of writting
    sheet.getRange("A4:C13").setValues(topsData[0]);
    sheet.getRange("A17:C26").setValues(topsData[1]);
    sheet.getRange("A30:C39").setValues(topsData[2]);
    sheet.getRange("A43:C52").setValues(topsData[3]);
    sheet.getRange("A56:C65").setValues(topsData[4]);
    sheet.getRange("A69:C78").setValues(topsData[5]);
  }
}