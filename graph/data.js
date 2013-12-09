var data = [];

var elements = 400

var width = 1600;
    height = 900;

var nodeIDs;

  function readSingleFile(evt) {
    //Retrieve the first (and only!) File from the FileList object
    var f = evt.target.files[0]; 

    if (f) {
      var r = new FileReader();
      r.onload = function(e) { 
	      var contents = e.target.result;
        console.log(contents)
        alert( "Got the file \n" 
              +"name: " + f.name + "\n"
              +"type: " + f.type + "\n"
              +"size: " + f.size + " bytes\n"
        );  
        nodeIDs = getData(contents)
        initialize();
        paint();
      }
      r.readAsBinaryString(f);

    } else { 
      alert("Failed to load file");
    }
  }

  document.getElementById('fileinput').addEventListener('change', readSingleFile, false);

/*
 * Generate samples
 */
 // CIRCLE
 /*
var radius = 420;

for (var i=0; i<= elements; i++) {
	var x = 
	data.push({
		uid: 'Node' + i,
		location: "Location" + Math.round(Math.random()*20),
		next: 'Node' + (i+1),
		newnode: false,
		x: width  / 2 + Math.sin(Math.PI * 2 / elements * i) * radius,
	    y: height / 2 + Math.cos(Math.PI * 2 / elements * i) * radius,
		fixed: true
	});
} */

var rectWidth = 1400,
    rectHeight = 800;

function initialize() {
var range = 2 * (rectWidth + rectHeight);
// RECTANGLE
for (var i=0; i<= elements; i++) {
	var rectX;
	var rectY;
	var position = range * i / elements
	var nextUID;

	if (elements == i) {
		nextUID = 'Node0';
	}
	else {
        nextUID = 'Node' + (i+1);
	}
    
    if (position <= rectWidth) {
    	rectX = (width  - rectWidth ) / 2 + position;
    	rectY = (height + rectHeight) / 2;
    }
    else
    if (position <= rectWidth + rectHeight) {
    	rectX = (width  + rectWidth ) / 2;
    	rectY = (height + rectHeight) / 2 - (position - rectWidth);
    }
    else 
    if (position <= 2 * rectWidth + rectHeight) {
    	rectX = (width  + rectWidth ) / 2 - (position - rectWidth - rectHeight);
    	rectY = (height - rectHeight) / 2;
    }
    else
    {
    	rectX = (width  - rectWidth ) / 2;
    	rectY = (height - rectHeight) / 2 + (position - 2 * rectWidth - rectHeight);
    }

	data.push({
		uid: 'Node' + i,
		location: "Location" + Math.round(Math.random() * 20),
		next: nextUID,
		newnode: false,
		x: rectX,
	    y: rectY,
		fixed: true
	});
}

/*
data.push({
	uid: 'Node' + elements,
	location: "Location" + Math.round(Math.random()*20),
	next: 'Node0',
	x: width  / 2 + Math.sin(Math.PI * 2 / elements * elements) * radius,
	y: height / 2 + Math.cos(Math.PI * 2 / elements * elements) * radius,
	fixed: true
});
*/

transformToNewNode()
transformToNewNode()
transformToNewNode()
transformToNewNode()
transformToNewNode()
transformToNewNode()
transformToNewNode()
}

function transformToNewNode() {
	var random = Math.round(Math.random()*(elements - 1))
	data[random].newnode = true;
    if (data[random].x == (width - rectWidth) / 2) {
    	data[random].x = data[random].x + 20
    } 
    if (data[random].x == (width + rectWidth) / 2) {
    	data[random].x = data[random].x + 20
    } 
    if (data[random].y == (height - rectHeight) / 2) {
    	data[random].y = data[random].y + 20
    } 
    if (data[random].y == (height + rectHeight) / 2) {
    	data[random].y = data[random].y - 20
    } 

}

function getData(input) {
	 var bs = input;
     var nodeIDs = new Array();
     console.log(" : " + bs);
     for(var i = 0;!(bs === ""); i++) {
       	 nodeIDs[i] = bs.substring(0, 6);
       	 bs = bs.substring(6);
     }

     console.log(nodeIDs)

     return nodeIDs
}

/*
Syntax
  
var data = [
	{
		uid: '',
		location: '',
		next: ''
		newnode: true/false
	}
];
*/