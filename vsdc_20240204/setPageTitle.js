// JavaScript Document
function setPageTitle() {
// this part extracts the page name : pageName.html
	var thisPageName = window.location.pathname.substring(window.location.pathname.lastIndexOf('/') + 1);
	oldTitle = new String;
	oldDirTitle = new String;
	thisPageName = thisPageName.substring (0, (thisPageName.length-5));
	pageNameChunks = new Object;
	dirNameChunks = new Object;
	var xx = 0;
	var ii = 0;
	var stop = 0;
    while(!stop){
		pageNameChunkStart = thisPageName.indexOf("_");
		if (pageNameChunkStart != -1)  {
			pageNameChunks[xx] = thisPageName.slice(0,pageNameChunkStart); 
			thisPageName = thisPageName.slice(pageNameChunkStart+1,thisPageName.length);
		  	oldTitle += pageNameChunks[xx] + " ";
			xx++;
		}
		else {
			oldTitle += thisPageName;
			stop = 1;
		}
	}
	
  // This part extracts directory names
  	sURL = new String;
  	sURL2 = new String;
 	bits = new Object;
  	newBits = new Object;
  	var x = 0;
  	var stop = 0;
  	var dirStop = 0;
  	var newTitle =  "CDDIS";
  	sURL = location.href;
	sURL2 = location.href;
  	sURL = sURL.slice(7,sURL.length);
	sURL = sURL.substring (0, (sURL.length-5))
  	while(!stop){
		dirStop=0;
		ii=0;
		oldDirTitle="";
    	chunkStart = sURL.indexOf("/");
    	if (chunkStart != -1){
		thisDirName = sURL.slice(0,chunkStart);
		  if (thisDirName.indexOf("cddis") != 0) {
			  while (!dirStop) {
			  	dirNameChunkStart = thisDirName.indexOf("_");
				if (dirNameChunkStart != -1)  {
					dirNameChunks[ii] = thisDirName.slice(0,dirNameChunkStart); 
					oldDirTitle += dirNameChunks[ii] + " ";
					ii++;
				}
				else {
					oldDirTitle += thisDirName;
					dirStop = 1;
				}
				thisDirName = thisDirName.slice(dirNameChunkStart+1,thisDirName.length);
			  }
		  }
		  newTitle += oldDirTitle;		  
		  newTitle += " | ";
		}
		else{ 
			newTitle += oldDirTitle;		 
			stop = 1;
		}
		sURL = sURL.slice(chunkStart+1,sURL.length);
	}
	if (sURL2 == "http://cddis.gsfc.nasa.gov/index.html") {
		newTitle = "CDDIS | Crustal Dynamics Data Information System"; }
	else {newTitle += oldTitle; }
 	document.title=newTitle;
}

		