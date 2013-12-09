
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

function generateNodeData(data) {
  console.log("@-@")
console.log(data)
var range = 2 * (rectWidth + rectHeight);
var elements = data.length
// RECTANGLE
for (var i=0; i< elements; i++) {
	var rectX;
	var rectY;
	var position = range * i / elements
	var nextUID;
  var sameNode = data[0].Id.substring(0, data[0].Id.indexOf(":")) === data[i].Id.substring(0, data[i].Id.indexOf(":"));
  var location = data[i].Id.substring(data[i].Id.indexOf(":") + 1);

	if (elements == i + 1) {
		  nextUID = data[0].Id;
	}
	else {
       nextUID = data[i+1].Id;
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

	nodeData.push({
		uid: data[i].Id,
		next: nextUID,
		newnode: sameNode,
		x: rectX,
	    y: rectY,
		fixed: true
	});
}
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