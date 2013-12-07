var linkDistance = 20,
	nodename = 'uid';

var links = [],
	nodes = [];
var map = {};	

var size = data.length;

for (var i=0; i< size; i++) {
	var node = data[i];
	map[node.uid] = node;
}

for (var i=0; i< size; i++) {
	if(typeof map[data[i].next] == 'undefined') {
		data[i].next = null;
	}
	else {
		data[i].next = map[data[i].next];
	}
}
//console.log(data);
if (mode == 1) {
	nodes = data;	
	for (var i=0; i< size; i++) {
		if (nodes[i].next !== null) {
			links.push({
				source: nodes[i], 
				target: nodes[i].next 
			});
		}
	}
}
else {
	for (var i=0; i< size; i++) {
		var node = data[i];
		if(typeof map[node.location] == 'undefined') {
			nodes.push(node);
		    map[node.location] = node;
		}
		else if (node.newnode) {
			map[node.location].newnode = true;
		}
	}

	for (var i=0; i< size; i++) {
		var node = data[i];
		if (node.next !== null) {
			links.push({
				id: links.length,
				source: map[node.location], 
				target: map[node.next.location]
			});
		}
	}
	
	linkDistance = 300;
	nodename = 'location';
}
	
var width = 1500,
    height = 1000;

var color = d3.scale.category10();

var force = d3.layout.force()
    .nodes(nodes)
    .links(links)
    .size([width, height])
    .linkDistance(linkDistance)
    .charge(-150)
    .on("tick", tick).start();

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

svg.append("defs").selectAll("marker")
    .data(["arrow"])
	.enter().append("marker")
    .attr("id", function(d) { return d; })
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 15)
    .attr("refY", -1.5)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
	.append("path")
    .attr("d", "M0,-5L10,0L0,5");
	
var path = svg.append("g").selectAll(".link")
  .data(force.links())
  .enter()
  .insert("line", ".node")
  .attr("class", "link")
  //.attr("id", function(d) { return "link" + d.id; })
  .attr("marker-end", "url(#arrow)");
  /*.on("mouseover", function() {
		d3.select(this).classed("active", true );
	});*/

	  
var node = svg.append("g").selectAll(".node")
  .data(force.nodes())
  .enter()
  .append("circle")
  .attr("r", function(d) { return d.newnode?10:8; })
  .attr("class", function(d) { return d.newnode?'new':''; })
  .call(force.drag)
  .on("mouseover", function() {
		var data = d3.select(this).data();
		var links = [];
		
		var out = svg.selectAll(".link")
		.filter(function(d) { return d.source.uid == data[0].uid });
		out.classed("out", true);
		
		/*console.log(svg.selectAll(".node"));
		svg.selectAll(".node")
		.classed("out", true);*/
		
		svg.selectAll(".link")
		.filter(function(d) { return d.target.uid == data[0].uid })
		.classed("in", true);
	})
	.on("mouseout", function() {
		svg.selectAll(".link")
		.classed("out", false)
		.classed("in", false);
	});;


var text = svg.append("g").selectAll("text")
    .data(force.nodes())
	.enter().append("text")
    .attr("x", 8)
    .attr("y", ".31em")
    .text(function(d) { return d[nodename]; });

	
function tick() {
  node.attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })

  path.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });
	  
  text.attr("transform", transform)
}

function transform(d) {
  return "translate(" + d.x + "," + d.y + ")";
}