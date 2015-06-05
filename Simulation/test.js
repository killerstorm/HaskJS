var Simulation = require('./simulation.js')
var kernel     = require('./kernel.js')



var sim = new Simulation();

var issuer = sim.wallet('issuer');
var alice  = sim.wallet('alice');
var bob    = sim.wallet('bob');

console.log("Issuer's address:\t" + issuer.getAddress());
console.log("Alice's address:\t" +  alice.getAddress());
console.log("Bob's address:\t\t" +  bob.getAddress());










