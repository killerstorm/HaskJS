var Simulation = require('./simulation.js')
var kernel     = require('./kernel.js')
var bitcoin    = require('bitcoinjs-lib')
var _          = require('lodash')


const coinbaseTxId = '0000000000000000000000000000000000000000000000000000000000000000';
const coinbaseOutIndex = 0xFFFFFFFF;

var sim = new Simulation();

var uncolored = sim.wallets['uncolored'];
var alice  = sim.wallet('alice');
var bob    = sim.wallet('bob');

console.log("uncolored address:\t" + uncolored.getAddress());
console.log("Alice's address:\t" +  alice.getAddress());
console.log("Bob's address:\t\t" +  bob.getAddress());

var coinbaseTx = new bitcoin.Transaction();
coinbaseTx.addInput(coinbaseTxId, coinbaseOutIndex);
coinbaseTx.addOutput(uncolored.getAddress(), 25000000);
coinbaseTx.addOutput(bitcoin.scripts.nullDataOutput(
    new Buffer ('([] [0] 1) 1 [1000000]')), 0);

sim.addTx(coinbaseTx);
sim.addCoins([{"txid" : coinbaseTx.getId(), "index" : 0, "value" : 25000000}]);


alice.issueCoin(100000);


alice.send(10000, bob);

console.log('uncolored balance  = ' + uncolored.getBalance());
console.log('alice\'s balance   = ' + alice.getBalance());
console.log('bob\'s balance     = ' + bob.getBalance());
