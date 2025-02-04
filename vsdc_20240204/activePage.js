// JavaScript Document

function setActiveLI() {
  sURL2 = new String;
  sURL2 = location.href;
	  if (((sURL2.indexOf("cddis.eosdis.nasa.gov/index.html") != -1) || (sURL2.indexOf(".gsfc.nasa.gov/CDDIS_File_Upload_Documentation.html") != -1)|| (sURL2.indexOf(".gsfc.nasa.gov/CDDIS_File_Upload.html") != -1) || (sURL2.indexOf(".gsfc.nasa.gov/CDDIS_FileUpload\/") != -1)))
	  {
		  document.getElementById("home").setAttribute("class","selected"); 
	  }
 	  if ((sURL2.indexOf("/About\/") != -1) || (sURL2.indexOf("/News\/") != -1) || (sURL2.indexOf("404error.html") != -1)) {
		  document.getElementById("about").setAttribute("class","selected"); 
	  }
  	  if (sURL2.indexOf("Data_citation_and_acknowledgment.html") != -1) {
		  document.getElementById("cite").setAttribute("class","selected"); 
	  }
	  if (sURL2.indexOf("/Data_and_Derived_Products\/") != -1) {
		  document.getElementById("dataSets").setAttribute("class","selected"); 
	  }
  	  if (sURL2.indexOf("/Techniques\/") != -1) {
		  document.getElementById("techniques").setAttribute("class","selected"); 
	  }
  	  if (sURL2.indexOf("/Programs\/") != -1) {
		  document.getElementById("programs").setAttribute("class","selected"); 
	  }
  	  if (sURL2.indexOf("/Publications\/") != -1) {
		  document.getElementById("pubs").setAttribute("class","selected"); 
	  }
return;
  }

function setL2PageHeading() {
  sURL3 = new String;
  sURL3 = location.href;
  L2PageHeading = new String;
  if (sURL3.indexOf("/Data_and_Derived_Products/") != -1) {
	  L2PageHeading = "<div class=\"page_title\"><a href=\"index.html\">Data and Derived Products</a></div>";
  }
  else if (sURL3.indexOf("/Techniques/") != -1) {
	  L2PageHeading = "<div class=\"page_title\">Geodesy Techniques</div>";
  }
  else if (sURL3.indexOf("/About/") != -1) {
	  L2PageHeading = "<div class=\"page_title\">About CDDIS</div>";
  }
  else if (sURL3.indexOf("/Programs/") != -1) {
	  L2PageHeading = "<div class=\"page_title\">Programs</div>";
  }
  else if (sURL3.indexOf("/Publications/") != -1) {
	  L2PageHeading = "<div class=\"page_title\">Publications</div>";
  }
  else if (sURL3.indexOf("/News/") != -1) {
	  L2PageHeading = "<div class=\"page_title\">News</div>";
  }
 document.write(L2PageHeading);
  return;
}
// This function only handles the path and page name - 
// The code checks for underscores ( _ ) and replaces those with spaces. 
function setPageTitle() {
	// this section handles ONLY the page name which ends in .html
	// if the page is named "index", it will not show in the page title.
	var thisPageName = window.location.pathname.substring(window.location.pathname.lastIndexOf('/') + 1);
	oldTitle = new String;
	if (thisPageName.indexOf ("index.html") != -1)  {
		oldTitle = ""; }	
	else {
		thisPageName = thisPageName.substring (0, (thisPageName.length-5));
		pageNameChunks = new Object;
		pageNameChunkStart = thisPageName.indexOf("_");
		var xx = 0;
		var stop = 0;
        while(!stop){
			pageNameChunkStart = thisPageName.indexOf("_");
			if (pageNameChunkStart != -1)  {
				pageNameChunks[xx] = thisPageName.slice(0,pageNameChunkStart); 
				thisPageName = thisPageName.slice(pageNameChunkStart+1,thisPageName.length);}
			else {stop = 1;}
			xx++
		}
		  pageNameChunks[xx-1] = thisPageName;
		  for(var ii in pageNameChunks){
		  oldTitle += pageNameChunks[ii] + " ";
  	}
	}
// This part handles the path to the file.  It also replaces underscores ( _ ) with spaces.
  sURL = new String;
  bits = new Object;
  newBits = new Object;
  newBits[0]="";
  var x = 0;
  var stop = 0;
  var stop2 = 0;
  var newTitle =  "CDDIS";
  sURL = location.href;
  sURL = sURL.slice(7,sURL.length);
  chunkStart = sURL.indexOf("/");
  sURL = sURL.slice(chunkStart+1,sURL.length)
  while(!stop){
    chunkStart = sURL.indexOf("/");
    if (chunkStart != -1){
		// bits[x] are directory names
      bits[x] = sURL.slice(0,chunkStart); // bits[x] = everything between one / and the next /
//	    newBits[x] = bits[x];
	  	pagePathChunks = new Object;
		oldBits = new Object;
  		var y = 0;
		oldBits[0]="";
		pagePathChunksStart = bits[x];  // start with everything between first / and the next /
//		alert (pagePathChunksStart);
		var z = 0;
		var stop2 = 0;
        while(!stop2){
			pagePathChunkStart = bits[x].indexOf("_");  // position of first occurence of _. For Data_and_Derived_Products it is 4. so bits is Data.
				y++;
			if (pagePathChunkStart !== -1)  {
				pagePathChunks[z] = bits[x].slice(0,pagePathChunkStart); // get word between _s
				oldBits[y] = pagePathChunks[z];  // assign word to oldBits[y]
				bits[x] = bits[x].slice(pagePathChunkStart+1,bits[x].length); // bits[x] is now the directory name minus the first word
				newBits[x] = newBits[x] + " " + oldBits[y]; // first time through, newBits[x] = newBits[0] + space + first word. newBits[0] = ""
				}
			else if (y > 1){
					oldBits[y] = bits[x];
					newBits[x] = newBits[x] + " " + oldBits[y];
				stop2 = 1; }
			else {
				newBits[x] = bits[x];
				stop2 = 1;
				}
		}
      sURL = sURL.slice(chunkStart+1,sURL.length);

    }else{
      stop = 1;
    }
    x++;
 }
 alert (oldTitle);
  for(var i in newBits){
// the "|" is the separator in the title. Change to something else if you like
//	newTitle += " | ";
	newTitle += newBits[i];
  }
  alert(newTitle);
  alert(thisPageName.indexOf("index.html"));
  if (thisPageName.indexOf("index.html") != -1)  {
		newTitle += oldTitle; }	
//  else {
//  	newTitle += " | " + oldTitle;
//  }
  document.title=newTitle;
  return;
}


// function below clears the words "CDDIS Text Search" from the search box when user clicks in it - "onfocus"

function clearText()  {
	document.getElementById("query").value = "";
	return;
}

// function below pops up a new window containing the "SLR center of mass correction" Concept Diagram on http://ilrs-test.gsfc.nasa.gov/missions/spacecraft_parameters/center_of_mass.html. This was implemented on the site prior to the redesign of Sept. 2012. It was not implemented on that site because some users disable pop-up windows in their browser settings.
function changeMap(mapSelected) {
	mapsArray = new Object;
	mapsArray[0] = "world"
	mapsArray[1] = "africa"
	mapsArray[2] = "antarctica"
	mapsArray[3] = "asia"
	mapsArray[4] = "australia"
	mapsArray[5] = "california"
	mapsArray[6] = "europe"
	mapsArray[7] = "northAmerica"
	mapsArray[8] = "pacific"
	mapsArray[9] = "southAmerica"	
	mapID = new String;
	mapID = mapSelected
	var zx = 0;
	while(zx < 10){
		document.getElementById(mapsArray[zx]).setAttribute("class",""); 
		zx++
	}
	if (mapID == "world") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_symbols_world_robinson_v3.png"; 
		document.getElementById('selectedMap').alt = "Map of world showing locations of Laser ranging stations";
	  	document.getElementById(mapID).setAttribute("class","selected");
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_symbols_world_robinson_v3.png");
		document.getElementById("mapLink").setAttribute("alt", "Map of world showing locations of Laser ranging stations")}
	else if (mapID == "africa") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_symbols_africa_lambert_v2.png"; 
		document.getElementById('selectedMap').alt = "Map of Africa showing locations of Laser ranging stations";
	  	document.getElementById(mapID).setAttribute("class","selected");
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_symbols_africa_lambert_v2.png")}
	else if (mapID == "antarctica") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_antarctica_polar_v2.png";
		document.getElementById('selectedMap').alt = "Map of Antarctica showing locations of Laser ranging stations";
	  	document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_antarctica_polar_v2.png")}
	else if (mapID == "asia") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_asia_albers_v2.png"; 
		document.getElementById('selectedMap').alt = "Map of Asia showing locations of Laser ranging stations";
	  	document.getElementById(mapID).setAttribute("class","selected");
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_asia_albers_v2.png")}
	else if (mapID == "australia") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_australia_mercator_v2.png";	
		document.getElementById('selectedMap').alt = "Map of Australia showing locations of Laser ranging stations";
    	document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_australia_mercator_v2.png")}
	else if (mapID == "california") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_california_mercator_v2.png";
		document.getElementById('selectedMap').alt = "Map of California, U. S., showing locations of Laser ranging stations";
	  	document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_california_mercator_v2.png")}
	else if (mapID == "europe") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_europe_lambert_v2.png";
		document.getElementById('selectedMap').alt = "Map of Europe showing locations of Laser ranging stations";
		document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_europe_lambert_v2.png")}
	else if (mapID == "northAmerica") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_symbols_na_bellwire_v2.png";
		document.getElementById('selectedMap').alt = "Map of North Amerrica showing locations of Laser ranging stations";
		document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_symbols_na_bellwire_v2.png")}
	else if (mapID == "pacific") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_pacific_mercator_v2.png";
		document.getElementById('selectedMap').alt = "Map of the Pacific Ocean showing locations of Laser ranging stations";	
		document.getElementById(mapID).setAttribute("class","selected");
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_pacific_mercator_v2.png") }	else if (mapID == "southAmerica") {
		document.getElementById('selectedMap').src = "/images/sgp_sites_symbols_sa_lambert_v2.png";
		document.getElementById('selectedMap').alt = "Map of South America showing locations of Laser ranging stations";
		document.getElementById(mapID).setAttribute("class","selected"); 
		document.getElementById("mapLink").setAttribute("href", "/images/sgp_sites_symbols_sa_lambert_v2.png")}
}
function displayCodeList(listName)  {
	document.getElementById(listName).style.display = 'block';
	return;
}
function displayCodeList(listName)  {
	document.getElementById(listName).style.display = 'none';
	return;
}

function toggleSubmenu(selMenu) {
  // Get the DOM reference
  var menuId = selMenu;
  var imgId = selMenu + "Arrow";
  // Toggle
  
  if (document.getElementById(menuId).style.display == "none") {
	  document.getElementById(menuId).style.display = "block";
	  document.getElementById(imgId).src ="/images/downArrow.png"; }
  else {
	document.getElementById(menuId).style.display = "none"; 
	document.getElementById(imgId).src = "/images/rightArrow.png"; }
}

function toggleSubmenuFtp(selMenuFtp) {
//	alert(selMenuFtp);
  var menuIdFtp = selMenuFtp;
  var imgIdFtp = selMenuFtp + "Arrow";
  // set up arrays of menuIdFTP and imgIdFtp
// data/campaign array
  	var campaignArray = new Object;
  	campaignArray[0] = "GNSScampaign";
//
//	data/daily array
  	var dailyArray = new Object;
  	dailyArray[0] = "GNSSYYYY";
  	dailyArray[1] = "GNSSYYYYddd";
	dailyArray[2] = "GNSSYYYYddd_d";
	dailyArray[3] = "GNSSYYYYddd_g";
	dailyArray[4] = "GNSSYYYYddd_m";
	dailyArray[5] = "GNSSYYYYddd_n";
	dailyArray[6] = "GNSSYYYYddd_o";
	dailyArray[7] = "GNSSYYYYddd_s";
	dailyArray[8] = "GNSSYYYYbrdc";
//
// data/highrate arraay
  	var highrateArray = new Object;
  	highrateArray[0] = "GNSShighrateYYYY";
  	highrateArray[1] = "GNSShighrateYYYYddd";
	highrateArray[2] = "GNSShighrateYYYYddd_d";
	highrateArray[3] = "GNSShighrateYYYYddd_g";
	highrateArray[4] = "GNSShighrateYYYYddd_m";
	highrateArray[5] = "GNSShighrateYYYYddd_n";
	highrateArray[6] = "GNSShighrateYYYYddd_d_hh";
	highrateArray[7] = "GNSShighrateYYYYddd_g_hh";
	highrateArray[8] = "GNSShighrateYYYYddd_m_hh";
	highrateArray[9] = "GNSShighrateYYYYddd_n_hh";

//
// data/highrate arraay
  	var hourlyArray = new Object;
  	hourlyArray[0] = "GNSShourlyYYYY";
  	hourlyArray[1] = "GNSShourlyYYYYddd";
	hourlyArray[2] = "GNSShourlyYYYYddd_hh";
//	

	var zy = 0;
	
						// Get the DOM reference
  // Toggle
  
  if (document.getElementById(menuIdFtp).style.display == "none") {
	  document.getElementById(menuIdFtp).style.display = "block";
	  document.getElementById(imgIdFtp).src ="/images/downArrow.png";
		if (menuIdFtp == "GNSSdaily")  {
			while(zy < 9){
				menuIdFtp = dailyArray[zy];
				imgIdFtp = dailyArray[zy] + "Arrow";
				document.getElementById(menuIdFtp).style.display = "block";
				document.getElementById(imgIdFtp).src ="/images/downArrow.png";
				zy++;
			}
		}
	  	else if (menuIdFtp == "GNSShighrate")  {
			while(zy < 10){
				menuIdFtp = highrateArray[zy];
				imgIdFtp = highrateArray[zy] + "Arrow";
				document.getElementById(menuIdFtp).style.display = "block";
				document.getElementById(imgIdFtp).src ="/images/downArrow.png";
				zy++;
			}
		}
	  	else if (menuIdFtp == "GNSShighrate")  {
			while(zy < 10){
				menuIdFtp = highrateArray[zy];
				imgIdFtp = highrateArray[zy] + "Arrow";
				document.getElementById(menuIdFtp).style.display = "block";
				document.getElementById(imgIdFtp).src ="/images/downArrow.png";
				zy++;
			}
		}
	  	else if (menuIdFtp == "GNSShourly")  {
			while(zy < 4){
				menuIdFtp = hourlyArray[zy];
				imgIdFtp = hourlyArray[zy] + "Arrow";
				document.getElementById(menuIdFtp).style.display = "block";
				document.getElementById(imgIdFtp).src ="/images/downArrow.png";
				zy++;
			}
		}
	  
  }
		  
  else {
	document.getElementById(menuIdFtp).style.display = "none"; 
	document.getElementById(imgIdFtp).src = "/images/rightArrow.png"; }
}

function toggleCodeList(listName)  {
	var el = document.getElementById(listName)
	document.getElementById(listName).style.display == "block" ? document.getElementById(listName).style.display = "none" : 
	document.getElementById(listName).style.display = "block"; 
}

function showFormatCodeList(tableName)  {
		document.getElementById(tableName).style.display = "block";
}
function hideFormatCodeList(tableName)  {
		document.getElementById(tableName).style.display = "none";
}  

function toggleFAQAnswer (FAQNum){
	var FAQel = document.getElementById(FAQNum)
	document.getElementById(FAQNum).style.display == "block" ? document.getElementById(FAQNum).style.display = "none" : 
	document.getElementById(FAQNum).style.display = "block"; 
}
function closeAlert (el)  {
	document.getElementById(el).style.display = "none";
}
	
function addTopOfPageButton()  {
	  sURL5 = location.href;
	  if ((sURL5.indexOf("cddis.gsfc.nasa.gov\/Publications\/Presentations.html") != -1) || (sURL5.indexOf("cddis.gsfc.nasa.gov\/Publications\/Publications.html") != -1)) {
		document.write("<div style=\"margin: 1px auto; padding: 0; border: 2px solid black; width: 36px;height: 39px;\">");
		document.write("<img src=\"/css/images/top.png\" style=\"position:relative; top:0px; right:0px;\" />"); 
		document.write("</div>"); }
	return;
}
function openURS_Form() {
window.open("https://urs.eosdis.nasa.gov/users/new","_blank","width=1320,height=800,menubar=no,status=no,location=no,directories=no,toolbar=no,scrollbars=yes")
}

function toggleSatTableRows(name)  {
	var satArray;
	satArray=document.getElementsByClassName(name);
	numSats=satArray.length;
	for (var i = 0; i < numSats; i++) {
		if (satArray[i].style.display == "none")  {
			satArray[i].style.display = "table-row";
		}
		else  {
			satArray[i].style.display = "none";
		}
	}
}
	
