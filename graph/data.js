var data = [];

/*
 * Generate samples
 */
for (var i=0; i< 50; i++) {
	data.push({
		uid: 'Node' + i,
		location: "Location" + Math.round(Math.random()*20),
		next: 'Node' + (i+1),
		newnode: false
	});
}

data.push({
	uid: 'Node50',
	location: "Location" + Math.round(Math.random()*20),
	next: 'Node0'
});

data[Math.round(Math.random()*49)].newnode = true;

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