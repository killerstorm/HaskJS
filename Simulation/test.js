var Simulation = require('./simulation.js')
var kernel     = require('./kernel.js')
var bitcoin    = require('bitcoinjs-lib')
var _          = require('lodash')

var sim = new Simulation();

console.log(sim);
var uncolored = sim.wallets['uncolored'];
var alice  = sim.wallet('alice');
var bob    = sim.wallet('bob');

console.log("uncolored address:\t" + uncolored.getAddress());
console.log("Alice's address:\t" +  alice.getAddress());
console.log("Bob's address:\t\t" +  bob.getAddress());

console.log(sim.wallets)


alice.issueCoin(100000);

console.log("alice sends to bob ");
alice.send(20000, bob);

console.log('uncolored balance  = ' + uncolored.getBalance());
console.log('alice\'s balance   = ' + alice.getBalance());
console.log('bob\'s balance     = ' + bob.getBalance());

uncolored.issueCoin(1000000000000000);
console.log(uncolored.getBalance())
