//the DW Logger Class	
function DWLogger() {  	
	//define if show debug log messages
	this.DebugEnable = true;
	
	//write debug messages to console if DebugEnable
	this.debugMessage = function (aMessage) {
		if (this.DebugEnable) {
			try {
					if (window.console && window.console.log) {
						console.log(aMessage);
					}
			} catch (e) {
				//ignore log error
			}
		}
	};
	
	//write error messages in console(always)
	this.consoleError = function (aMessage) {
		if (window.console && window.console.error) {
			console.error(aMessage);
			return true;
		} else {
			return consoleWrite(aMessage);
		}
	};
	
	//write log messages in console(always)
	this.consoleWrite = function (aMessage) {
		if (window.console && window.console.log) {
			console.log(aMessage);
			return true;
		} else {
			return false;
		}
	};
}

//Create DW.logger Object
function DWInitLogger(){
    var logger = null;
	//already created
	if (DW){
		logger = DW.logger;
	
		if (!logger){
		  logger = new DWLogger;	
		}
		DW.logger = logger;
	}
}
