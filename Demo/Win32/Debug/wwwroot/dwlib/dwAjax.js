function DWControlChanged(event){
	DW.logger.debugMessage("Executing DWControlChanged Control:" + event.target)
	
	//check event
	if (!event) {
	  return false;
	}
	//get element of event
	var xElement;
	if (event.id){
		//event is already the source element
		xElement = event;
	} else {
		//event is a "event"
		xElement = event.target || event.srcElement;
	}
	if (!xElement) {
		return false;
	}
	//get element with ID
	var xId = xElement.id;
	while (xId == "") {
	  xElement = xElement.parentNode;
	  if (!xElement) {
		return false;
	  }
	  if (xElement.id) {
		xId = xElement.id;
	  }
	}
	

	/* //TODO
	// handle implicit submit when pressing ENTER inside a text input
	if (event.type === "keypress") {
	if (isTextInput(xElement) && IW.implicitSubmit && xElement.ExtendedProps && xElement.ExtendedProps.ImplicitSubmit) {
		var submitted = ExecuteImplicitSubmit(event, xElement.id, xElement.ExtendedProps.SubmitValidation);
		return !submitted;  // only continue if not submitted!
	}
	} 
	*/

	AddToChangedList(xElement);
	
	/*  TODO
	//Radiogroups have a nested input control which carries the actual value
	if (isRadioGroup(xElement)) {
	AddChangedControl(xControlName+"_INPUT");
	}
	*/
	return true;
} 

function AddToChangedList(aControl) {
  DW.logger.debugMessage('changed control: ' + aControl.id);
  //verify before add
  if ((aControl) && !ControlIsChanged(aControl)) {
    window.ChangedList.push(aControl);
    DW.logger.debugMessage('changed control added: ' + aControl.id);
  }
}

function ControlIsChanged(aControl){
	return GetChangeListIndex(aControl) >= 0;
}

function GetChangeListIndex(aControl)
{
	return window.ChangedList.indexOf(aControl);
}


function PrepareSubmitForm(){
	
	if (!DW.form) {
		return false;
	}
	//remove all items with class 'DWSUBMIT'
	$(DW.form).find(".DWSUBMIT").remove();
	
	//get changed controls from ChangedList
	var xChanged = window.ChangedList;
	
	//add changed controls to submit form "DWAjaxForm"
	//reverse each to first add last changed controls
	//LIFO --> Yes, I remember of computer science lessons :|). 
	$(xChanged.reverse()).each(function(){
		$('<input>').attr({
			type: 'hidden',
			//id: this.id,
			name: this.id,
			class: "DWSUBMIT", 
			value: this.value
		}).appendTo(DW.form);
		
	});
	return true;
}

function SubmitForm(event){
	if (!DW.form) {
		event.preventDefault();
		DW.logger.consoleError("DW.form not found");
		return false;
	}

	if (PrepareSubmitForm()){
		DW.form.action = $(location).attr('href');
		event.preventDefault();
		var xdata = $(DW.form).serialize();
		if (xdata) {
			$.post( DW.form.action, xdata, null, "json")
				.done(DoneSubmit)
				.fail(FailSubmit)
				.always(AlwaysSubmit);
		}
		return false;
	} else {
		DW.logger.consoleError('Error in PrepareSubmitForm');
		return false;
	}
}

//process data received in callback
function DoneSubmit(data) {
	DW.logger.debugMessage('Done post. Data received: ' + data);
	var executeFirstXml;
	var updateXml;
	var executeXml;
	executeFirstXml = data.childNodes[0].childNodes[0];
	updateXml = data.childNodes[0].childNodes[1];
	executeXml = data.childNodes[0].childNodes[2];
	//process executeFirst part
	processXml(executeFirstXml);
	//process update part
	processXml(updateXml);
	//process execute part
	processXml(executeXml);	
	window.ChangedList = [];
}

function FailSubmit(data) {
	DW.logger.consoleError('FAil post. Data received: ' + data);		
}

function AlwaysSubmit(data) {
	//none for now		
}

function executeAjaxCallBack(eventParams, aSender, aCallback){
	if (!DW.form) {
		DW.logger.consoleError("DW.form not found");
		return false;
	}
	//add changed contros to submit form
	if (PrepareSubmitForm()){
		/* add eventParams to submit form */
		/*$(eventParams).each(function(){
			$('<input>').attr({
				type: 'hidden',
				//id: this.id,
				name: this.Name,
				class: "DWSUBMIT", 
				value: this.Value
			}).appendTo(DW.form);
		});*/
		//add sender to form
		if (aSender){
			$('<input>').attr({
				type: 'hidden',
				//id: this.id,
				name: "Sender",
				class: "DWSUBMIT",
				value: aSender.id || aSender.name
			}).appendTo(DW.form);
		};
		//set post url
        if (eventParams){
		    DW.form.action = $(location).attr('href') + '?callback=' + aCallback + eventParams;
        } else {
            DW.form.action = $(location).attr('href') + '?callback=' + aCallback;
        }
        //get form data
		var xdata = $(DW.form).serialize();
		if (xdata) {
			//send to server
			$.post( DW.form.action, xdata, null, "xml")
				.done(DoneSubmit)
				.fail(FailSubmit)
				.always(AlwaysSubmit);
		}
		return false;
	} else {
		DW.logger.consoleError('Error in PrepareSubmitForm');
		return false;
	}	

}



function SendPostRequest(eventParams, aSender, aCallback) {
   var xSubmitForm = PrepareSubmitter(aSender);

   var aURL = GURLBase + 'callback' + '?callback=' + aCallback;

   window.ChangedControls = "";

   xSubmitForm.action = aURL + eventParams;

   // this block only emmits debug information
   if (IWDEBUG) {
     logMessage('Ajax Submitting Form : ' + xSubmitForm);
     logMessage('Action : ' + xSubmitForm.action);
     logMessage('Elements: ' + xSubmitForm.childNodes.length);
     for (var i=0; i<xSubmitForm.childNodes.length; i++ ) {
   	   var xItem = xSubmitForm.childNodes.item(i);
   	   logMessage(xItem.name + ' = ' + xItem.value);
     }
   }

   if (PreAsyncScript == true) {
     PreAsyncProgressScript(eventParams,aCallback);
   }
   var status = AjaxRequest.submit(
     xSubmitForm, {
       'onSuccess':function(req)
          {
            AsyncReleaseLock();
            var xmldoc;
            xmldoc = loadAjaxResponse(req.responseText);
            processAjaxResponse(xmldoc);
			xmldoc = null;
            if (PostAsyncScript == true) {
              PostAsyncProgressScript(eventParams,aCallback);
            }
            processEventQueue();
            window.eventProcessing = false;
          }
        ,'onError':function(req) {
            AsyncReleaseLock();
            if (PostAsyncScript == true) {
              PostAsyncProgressScript(eventParams,aCallback);
            }
            window.eventProcessing = false;
            if (req.responseText == "") {
              HandleServerProblem(req);
            } else {
              WriteToDoc(req.responseText);    // create a new document and write the response to that page
            }
            logMessage('Ajax-Error!\nStatusText='+req.statusText+'\nContents='+req.responseText);
        }

     }
   );
   //restore old action
   // xSubmitForm.action = oldAction;

   return status;
 }

/*function loadResponseXml(aData) {
  if (window.ActiveXObject) {//if IE
    var lXml = new ActiveXObject("Microsoft.XMLDOM");
    lXml.async = false;
    lXml.loadXML(aResponse);
  } else { // Mozilla
    var lDomParser = new DOMParser();
    var lXml = lDomParser.parseFromString(aResponse, "text/xml");
    lDomParser = null;
  }
  return lXml;
}*/ 

function processXml(xmlResp){
   $(xmlResp).each(function () {
		DW.logger.debugMessage($(this).text());
		eval($(this).text());
   });   
}


function AsyncRenderControl(id, parentSelector, htmlTag) {
	var elem = document.getElementById(id);
	if (elem === null && parentSelector === "") {
		return false;
	}
	if (elem === null) {
		var scrp = $(parentSelector).children('script');
		if (scrp.length === 0) {
			$(parentSelector).append(htmlTag);
		} else {
			$(scrp[0]).before(htmlTag);
		}
	} else {
		$("#" + id).replaceWith(htmlTag);
	}
   /*	var formTag = $("body > form[name='DWAjaxForm']");
	var hidInpt;
	$("#" + id).find('input, select, textarea').each(function () {
		hidInpt = $(formTag).find("input[name='" + this.name + "']");
		if (hidInpt.length === 0) {
			formTag.append('<input type="hidden" name="' + this.name + '">');
		}
	});
	if (typeof webshims !== "undefined") {
		$("#" + id).updatePolyfill();
	}  */
}

function AsyncDestroyControl(id) {
   /*	var elem = document.getElementById(id);
	if (elem !== null) {
		var formTag = $("body > form[name='SubmitForm']");
		var hidInpt;
		$(elem).find('input').each(function () {
			$(formTag).find("input[name='" + this.name + "']").remove();
		});
	} */
	$("#" + id).remove();
   /*	var iwelem = window[id + "IWCL"];
	if (iwelem !== null) {
		iwelem.IWRelease();
		iwelem.remove();
	} */
}



function AsyncLoadJsCss(filename, filetype){
    if (filetype=="js"){ //if filename is a external JavaScript file
	    var fileref=document.createElement('script')
        fileref.setAttribute("type","text/javascript")
        fileref.setAttribute("src", filename)
		fileref.setAttribute("id", "test");
		document.getElementsByTagName("head")[0].appendChild(fileref);
		eval(document.getElementById("test").innerHTML);
    }
    else if (filetype=="css"){ //if filename is an external CSS file
        var fileref=document.createElement("link")
        fileref.setAttribute("rel", "stylesheet")
        fileref.setAttribute("type", "text/css")
        fileref.setAttribute("href", filename)
    }
    if (typeof fileref!="undefined")
        document.getElementsByTagName("head")[0].appendChild(fileref);
}


function AsyncReplaceJsCss(oldfilename, newfilename, filetype){
    var targetelement=(filetype=="js")? "script" : (filetype=="css")? "link" : "none" //determine element type to create nodelist using
    var targetattr=(filetype=="js")? "src" : (filetype=="css")? "href" : "none" //determine corresponding attribute to test for
    var allsuspects=document.getElementsByTagName(targetelement)
    for (var i=allsuspects.length; i>=0; i--){ //search backwards within nodelist for matching elements to remove
        if (allsuspects[i] && allsuspects[i].getAttribute(targetattr)!=null && allsuspects[i].getAttribute(targetattr).indexOf(oldfilename)!=-1){
            var newelement=createjscssfile(newfilename, filetype)
            allsuspects[i].parentNode.replaceChild(newelement, allsuspects[i])
        }
    }
}

function AsyncRemoveJsCss(filename, filetype){
    var targetelement=(filetype=="js")? "script" : (filetype=="css")? "link" : "none" //determine element type to create nodelist using
    var targetattr=(filetype=="js")? "src" : (filetype=="css")? "href" : "none" //determine corresponding attribute to test for
    var allsuspects=document.getElementsByTagName(targetelement)
    for (var i=allsuspects.length; i>=0; i--){ //search backwards within nodelist for matching elements to remove
        if (allsuspects[i] && allsuspects[i].getAttribute(targetattr)!=null && allsuspects[i].getAttribute(targetattr).indexOf(filename)!=-1){
            //var newelement=createjscssfile(newfilename, filetype)
            allsuspects[i].remove();
        }
    }
}


 