//Class declaration for DWEventProcessor
function DWEventProcessorClass(aComponent){
	//Set the Owner of this
	this.Owner = aComponent;
	
	//retain the last Event found in GetEventIndexByName for SpeedUp
	lastFound = 0;
	
	eventTypes = ["abort","change"];
		
	
	//EventHandlers is a bidirectional array correspond with:
	//EventHandlers[<"eventTypes" index >][<EventHandler Function>]
	this.EventHandlers = [];
			
	
	/*	this function process all events hookeds and redirect to its eventHandlerFunctions
		!!!! Attention !!!!: this function is called by DOM Element, 
		consequently "this" represent a DOM element and not EventProcessor Object. */
	this.ProcessEvent = function (aEvent, a2, a3, a4) {
		var result = true;
		DW.logger.debugMessage("EventProcessor: Processing Event");
		try {
			//get event source
			var origEvent = aEvent || window.event;
			if (origEvent) {
			DW.logger.debugMessage("EventProcessor: origEvent.type = " + origEvent.type);
			} else {
			DW.logger.debugMessage("EventProcessor: origEvent not found");
			}
			//get index of event type
			var i = this.EventProcessor.GetEventIndexByName(origEvent.type);
			if (i < 0) {
			return result;
			}
			//Get the DWControl of the element that originated the event
			var origControl = DW.controls.GetControlByElement(origEvent.target); 
			//if found DWControl
			if (origControl != null) {
				/* if (origControl.IsAsyncEventsDisabled()) {     // if DWC async events were disabled by user, return
				   logMessage("EventProcessor: origControl (" + origControl.id + ") is disabled for async events.");
				   return true;
				}  */
				//get the Hook for origControl of represent this event
				var CompHook = origControl.EventProcessor.EventHandlers[i];
				//if found Hook
				if (CompHook) {
					//Loop for all eventHandlerFunctions of this Hook
					var count = CompHook.length;
					for (var j = 0; j < count; j++) {
						DW.logger.debugMessage("EventProcessor: Executing event handler Function");
						origControl.eventFunc = CompHook[j];
						//try execute current eventHandlerFunction
						if (!origControl.eventFunc(origEvent)) {
							DW.logger.consoleError("EventProcessor: Err in execute Eventhandler");
							result = false;
							break;
						}
					}
				}
			} else {
				DW.logger.debugMessage("EventProcessor: origControl is null");
			}
		}
		catch (e) {
			DW.logger.consoleError(e.name + ": " + e.message);
			//var trace = printStackTrace({e: e});
			//consoleWrite(trace);
			throw e;
		}
		return result;
	}
	
	//Get the event index by event name
	this.GetEventIndexByName  = function (aEventName){
		if (eventTypes[this.lastFound] == aEventName) {
			return this.lastFound;
		}
		var result = -1;
		for (var i = 0; i < eventTypes.length; i++) {
			if (eventTypes[i] == aEventName) {
				result = i;
				break;
			}
		}
		this.lastFound = result;
		return result;
	}
	
	//Hook Component Event
	this.HookEvent = function (eventName, eventHandlerFunction) {
		//Get the "eventType" index
		var i = this.GetEventIndexByName(eventName);
		DW.logger.debugMessage("EventProcessor: HookEvent: eventName = " + eventName);
		//create a hook for this eventName
		if (this.EventHandlers[i] == null) {
			this.EventHandlers[i] = [];
			//get element event name
			var xEventName = "on" + eventName;
			//if Compenent has original event
			if (this.Owner[xEventName] != null) {
				//Hook Abort to Compenent original event
				this.EventHandlers[0] = this.Owner[xEventName]
			}
			//Hook Compenent original event to EventProcessor.ProcessEvent
			this.Owner[xEventName] = this.ProcessEvent;
		}
		//add eventHandlerFunction to Hook
		this.EventHandlers[i].push(eventHandlerFunction);
		return this;
	}


	//UnHook Component Event
	this.UnHookEvent = function(eventName, eventHandlerFunction){
		//Get the "eventType" index
		var i = this.GetEventIndexByName(eventName);
        DW.logger.debugMessage("EventProcessor: UnHookEvent: eventName = " + eventName);
        //Verify if Hook exists for this eventName 
		if (this.EventHandlers[i] == null ) {
            //Exit if Hook not exists
			return this;
        }
        //Find eventHandlerFunction for this Hook
		var iLen = this.EventHandlers[i].length;
        for (var j = 0; j < iLen; j++) {
            //if found
			if (this.EventHandlers[i][j] == eventHandlerFunction) {
                //remove eventHandlerFunction
				this.EventHandlers[i].splice(j, 1);
                break;
            }
        }
        return this;
	}	
}

//Create a DWComponent.EventProcessor Object
function DWInitEventProcessor(aComponent){	
	//verify already created and if not Create
	var lEvtProc = null;
	if (aComponent) {
		lEvtProc = aComponent.EventProcessor;
		if (!lEvtProc){
			lEvtProc = new DWEventProcessorClass(aComponent);
		}
		aComponent.EventProcessor = lEvtProc;
	}	
}