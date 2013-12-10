var nodeData = [];

var width = 1600;
    height = 900;

var node;



	
d3.json("../golang/peer/graph.json", function(data) {

console.log("-----")
console.log(data)
generateNodeData(data)

var linkDistance = 5,
	nodename = 'uid';

var links = [],
	nodes = [];
var map = {};


//console.log(data);
	nodes = data;	
	for (var i=0; i< size; i++) {
		if (nodes[i].next !== null) {
			links.push({
				source: nodes[i], 
				target: nodes[i].next 
			});
		}
	}

var size = data.length;

	var color = d3.scale.category10();

var force = d3.layout.force()
    .nodes(nodeData)
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

console.log("Test")
console.log(nodeData)

node = svg.append("g").selectAll(".node")
  .data(nodeData)
  .enter()
  .append("circle")
  .attr("r", function(d) { return d.newnode?10:2; })
  .attr("class", function(d) { return d; })
  //.attr("cx", function(d) { return d.y})
  //.attr("cy", function(d) { return d.y})
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
    .data(nodeData.filter(function(d) { return d.newnode}))
	.enter().append("text")
    .attr("x", function(d) {return d.x + 14})
    .attr("y", function(d) {return d.y })
    .text(function(d) { return d[nodename]; });
	

})

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
