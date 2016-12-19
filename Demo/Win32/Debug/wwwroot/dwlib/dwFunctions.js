//DWForm Class declaration
function DWFormClass(){
	//public propertyes for DW.form
	
}

//Initialize DW.form Object
function DWInitForm(){ 
	DWReleaseForm(); 
	DWCreateForm(); 	
}

//Release DW.form Object
// !!!!**** Do not call this directly, always call DW.form.DWRelease ****!!!!
function DWReleaseForm(){ 
	frm = DW.form;
	if (frm) {
		var i;
		for (i = 0; i < frm.OwnedComps.length; i++) {
			if (frm.OwnedComps[i].DWRelease) {  
				frm.OwnedComps[i].DWRelease();
			}
			frm.OwnedComps[i] = null;
		}	
		if (frm.oldCursors) {
			for (i = 0; i < frm.oldCursors.length; i++) {
				frm.oldCursors[i] = null;
			}
		
			for (i = 0; i < frm.HTMLelements.length; i++) {
				frm.HTMLelements[i] = null;
			}
		}	
	}		
	frm = null;
	DW.form  = null;
}
	
//Create DW.form Object
// !!!!**** Do not call this directly, always call DWInitForm()****!!!!
function DWCreateForm(){ 
	var frm = $("#DWAjaxForm")[0];
	if (DW){
		if(!frm){
			frm = new DWFormClass;
		}
		frm.Item = document.body;
		frm.OwnedComps = [];
		frm.DWRelease = DWReleaseForm;
		$(frm).on("submit", SubmitForm);
		DW.form = frm;
	}
	
}

//return DWObject for an HTML Element
function DWFindObject(element) {
    if (!element) {
        return null;
    }
    var xObj;
    var el = element;
    do {
        xObj = window[el.id + "DWC"];
        if (xObj != null) {
            return xObj;
        } else {
            el = el.parentNode;
        }
    }
    while (el != null);
}

//return an form by name property
function DWGetFormByName(aFormName){
	return window.document.forms[aFormName];
}

// hooks or unhook Input events of an element
function DWSetSupportsInput(aControl, aValue) {
	$(aControl).off("change.DW", DWControlChanged);
    $(aControl).off("keypress.DW", DWControlChanged);
    $(aControl).off("click.DW", DWControlChanged);
    $(aControl).off("select.DW", DWControlChanged);    
    if (aValue === true){
        $(aControl).on("change.DW", DWControlChanged);
        $(aControl).on("keypress.DW", DWControlChanged);
        $(aControl).on("click.DW", DWControlChanged);
        $(aControl).on("select.DW", DWControlChanged);
    }
}








