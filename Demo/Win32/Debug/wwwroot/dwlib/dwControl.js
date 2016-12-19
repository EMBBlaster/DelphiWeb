//Initialize Control Object
function DWInitControl(AOwner, AId, AVisibility, ASupportsInput){
	
	var Item = $("#" + AId)[0];

	if(Item != null) {
		if (ASupportsInput) {
			DWSetSupportsInput(Item, true);
		}
	}
	return 
}



