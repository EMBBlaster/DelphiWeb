/*------------------------------------------------------------------------------
DW namespace
------------------------------------------------------------------------------*/
var DW = {
    //DW.browser return Browser Object
	browser:null,
	
	//ajax submit form
	form:null,
	

	
	//log and debug
	logger:null,
	
	
	//Initialize de DW Objects
	initDW : function() {
				//init List of changed controls to submit only controls changed
				window.ChangedList = [];
				DWInitLogger();
				DWInitBrowser();  
				DWInitForm();
			}
	
}
