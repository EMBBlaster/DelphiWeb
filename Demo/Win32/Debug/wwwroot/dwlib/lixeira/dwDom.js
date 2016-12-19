DW.DOM = {

    getSubmitElement: function(elementName) {
        var xSubmitForm = DW.getSubmitForm();
        if (xSubmitForm){
            for (var j=0; j < xSubmitForm.elements.length; j++) {
                if (elementName == xSubmitForm.elements[j].name) {
                    return xSubmitForm.elements[j];
                }
            }
        }
        return null;
    },

    setSubmitElementValue: function(elementName, elementValue) {
        var el = DW.DOM.getSubmitElement(elementName);
        if (el) {
            el.value = elementValue;
            return true;
        }
        return false;
    },

    setBounds: function(elementId, top, left, height, width) {
        var el = DW.$(elementId);
        if (el && el.style) {
            if (top != null) {
                setCssRuleAttr(el, "top", top + "px");
            }
            if (left != null) {
                setCssRuleAttr(el, "left", left + "px");
            }
            if (height != null) {
                setCssRuleAttr(el, "height", height + "px");
            }
            if (width != null) {
                setCssRuleAttr(el, "width", width + "px");
            }
            setCssRuleAttr(el, "position", "absolute");
        }
    },

    // resize an element to heightPercent & widthPercent of browser window sizes
    proportionalResize: function(elementId, heightPercent, widthPercent) {
        var el = DW.$(elementId);
        if (el && el.style) {
            var wSizes = DW.browser.WindowSizeXY();
            if (heightPercent != null) {
                var height = Math.floor(wSizes[1] * heightPercent);
                setCssRuleAttr(el, "height", height + "px");
            }
            if (widthPercent != null) {
                var width = Math.floor(wSizes[0] * widthPercent);
                setCssRuleAttr(el, "width", width + "px");
            }
        }
    },

    getElementHeight: function(elementId) {
        var el = DW.$(elementId);
        if (el && el.offsetHeight) {
            return el.offsetHeight;
        } else {
            return 0;
        }
    },

    getElementWidth: function(elementId) {
        var el = DW.$(elementId);
        if (el && el.offsetWidth) {
            return el.offsetWidth;
        } else {
            return 0;
        }
    },

    createElement: function(tagName, id, parentId, innerHTML, insertBefore) {
      var element = window.document.createElement(tagName);
      element.id = id;
      if (innerHTML != null) {
        element.innerHTML = innerHTML;
      }
      var parent;
      if (parentId != null) {
        parent = DW.$(parentId);      // parentId may be the element id or the object itself
      } else {
        parent = document.body;
      }
      if (insertBefore && parent.firstChild) {
        parent.insertBefore(element, parent.firstChild);
      } else {
        parent.appendChild(element);
      }
      return element;
    },

    createDiv: function(id, parentId, innerHTML) {
	    return DW.DOM.createElement("div", id, parentId, innerHTML);
    },

    createOverlay: function(id, transparent){
        // overlay div
        var div = DW.DOM.createDiv(id);
        var style = "z-index: 20000;";
        if (DW.browser.IsIE && (DW.browser.IEDocumentMode <= 9)) {   // will use filter to do IE9 as well
            var color = (transparent ? "#00000000" : "#4C000000");
            // IE7 and 8 transparent effect
            style = style + "filter: progid:DXImageTransform.Microsoft.gradient(startColorstr=" + color + ",endColorstr=" + color + ");" +
                    "background: url(data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7)";
        } else {
            // W3C
            var alpha = (transparent ? "0.0" : "0.3");
            style = style + "background-color: rgba(0, 0, 0, " + alpha + ")";
        }
        var className = defaultClassName(id);
        addClass(div, className);
        if (!DW.DOM.replaceCssStyle("." + className, style)) {
            var el = DW.DOM.createStyleElement();
            DW.DOM.addCssStyle(el, "." + className, style);
        }
        return div;
    },

    createInput: function(id, type, parentId) {
        var element = window.document.createElement("input");
        element.id = id;
        element.type = type;
        var parent;
        if (parentId != null) {
          parent = DW.$(parentId);      // parentId may be the element id or the object itself
        } else {
          parent = document.body;
        }
        parent.appendChild(element);
        return element;
    },

    createSubmit: function() {
        var el = DW.DOM.createInput("DW_Default_Submit", "submit", DW.getSubmitForm());
        el.style = "position: absolute; left: -9999px; width: 1px; height: 1px;";
    },

    // Creates a hidden input element and append it to the form
    createHiddenField: function(formName, name, value) {
        var el;
        var d = window.document;
        if (DW.browser.IsIE && DW.browser.IEDocumentMode < 8) {
            try {
              el = d.createElement("<input name='"+name+"'>");   // blame MS for that! There is no other way to create named elements in IE7
            } catch (e) {
              el = d.createElement("input");  // fall back if somehow the browser is not really IE7
            }
        } else {
            el = d.createElement("input");
        }
        el.type = "hidden";
        el.name = name;
        el.value = value;
        var form = DWGetFormByName(formName);
        if (form) {
            form.appendChild(el);
        }
    },

    createStyleElement: function() {
        var el = window.document.createElement("style");
        DW.getElementByTagName("head").appendChild(el);
        if (el.setAttribute) {
            el.setAttribute("type", "text/css");
        }
        return el;
    },

    addCssStyle: function(styleElement, selectorName, style) {
        if (styleElement.styleSheet) {
            styleElement.styleSheet.cssText += selectorName + " {" + style + "}" + "\n";   //IE7 and 8
        } else {
            styleElement.innerHTML += selectorName + " {" + style + "}" + "\n";   // W3C
        }
        return styleElement;
    },

    // utility functions to add JS code and CSS styles to DOM at runtime
    addJsCode: function(code) {
        var el = document.createElement("script");
        DW.getElementByTagName("head").appendChild(el);
        if (el.setAttribute) {
            el.setAttribute("type", "text/javascript");
        }
        el.innerHTML = code;
        return true;
    },

    loadJsFile: function(src) {
        var el = document.createElement("script");
        DW.getElementByTagName("head").appendChild(el);
        if (el.setAttribute) {
            el.setAttribute("type","text/javascript");
            el.setAttribute("src", src);
        }
        return true;
    },

    loadCssFile: function(href) {
        var el = document.createElement("link");
        DW.getElementByTagName("head").appendChild(el);
        if (el.setAttribute) {
            el.setAttribute("rel", "stylesheet");
            el.setAttribute("type", "text/css");
            el.setAttribute("href", href);
        }
        return true;
    },

//    addCssStyle: function(selectorName, style) {
//        var el = IW.DOM.createStyleElement();
//        if (IW.String.isEmpty(style)) {
//            if (el.styleSheet) {
//                el.styleSheet.cssText = selectorName + " {" + style + "}";   //IE7 and 8
//            } else {
//                el.innerHTML = selectorName + " {" + style + "}";   // W3C
//            }
//        }
//        return el;
//    },

    cssExists: function(selectorName) {
        var styles = DW.getStyleTagElements();
        for (var i = 0; i < styles.length; i++) {
          if (styles[i].innerHTML.indexOf(selectorName) >= 0) {
            return true;
          }
        }
    },

    replaceCssStyle: function(selectorName, style) {
        var styles = DW.getStyleTagElements();
        var el;
        for (var i = 0; i < styles.length; i++) {
          el = styles[i];
          if (el.innerHTML.indexOf(selectorName) >= 0) {
            if (el.styleSheet) {
                el.styleSheet.cssText = selectorName + " {" + style + "}";   //IE7 and 8
            } else {
                el.innerHTML = selectorName + " {" + style + "}";
            }
            return true;
          }
        }
    },

    forceRedraw: function(element){
        var el = DW.$(element);
        if (el) {
            var x = el.offsetHeight;
            var disp = el.style.display;
            // TODO: use the CSS rule instead of inline display
            el.style.display = "none";
            el.style.display = disp;
        }
    }
}
