var Simulation = require('./simulation.js')
var kernel     = require('./kernel.js')
var bitcoin    = require('bitcoinjs-lib')


const coinbaseTxId = '0000000000000000000000000000000000000000000000000000000000000000';
const coinbaseOutIndex = 0xFFFFFFFF;

var sim = new Simulation();



var issuer = sim.wallet('issuer');
var alice  = sim.wallet('alice');
var bob    = sim.wallet('bob');

console.log("Issuer's address:\t" + issuer.getAddress());
console.log("Alice's address:\t" +  alice.getAddress());
console.log("Bob's address:\t\t" +  bob.getAddress());

var coinbaseTx = new bitcoin.Transaction();
coinbaseTx.addInput(coinbaseTxId, coinbaseOutIndex);







