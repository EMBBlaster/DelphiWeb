//DWControls Class
function DWControls(){
	var FItems = [];
	
	//array of DWControl Objects
	this.Items = FItems;
	
	//Get the DWObject for an element
	this.GetControlByElement = function (aElement){
		if (!aElement) {
			return null;
		}
		var lControl;
		do {
			lControl = this.FItems[aElement.id + "DWC"];
			if (lControl != null) {
				return lControl;
			} else {
				aElement = aElement.parentNode;
			}
		}
		while (aElement != null);
	}	
}

//Create the DW.controls Object
function DWInitControls(){
	var controls = null;
	//check if already created
	if (DW) {
		controls = DW.controls;
		if (!controls){
			controls = new DWControls;
		}
		DW.controls = controls;
	}
}



	