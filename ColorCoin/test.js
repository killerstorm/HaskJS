const ck = require ('./src/ckernel.js');
const h = require('./main.js').getHaste();

const bc = require('bitcoinjs-lib');
const bt = require('buffertools');
const crypto = require('crypto');
const _ = require('underscore');


var Transaction = bc.Transaction;

var unspent = [];


var test_payload = new Buffer("([], [0], 1) 1 [1000]");
var test_tx = new Transaction();
test_tx.addOutput(bc.scripts.pubKeyHashOutput(crypto.randomBytes(20)), 0);
test_tx.addOutput(bc.scripts.nullDataOutput(new Buffer(test_payload)), 0);


var tx = [];
tx.push(ck.createTx(test_tx));

unspent = unspent.concat(h.runCoinKernelOnGraph(tx));
//console.log(unspent);

var transactions = [];
var tx_graph = [];
var coins = [];
tx_graph.push(ck.createTx(test_tx));

console.time("running coinKernel...");

for (var i = 0; i < parseInt(process.argv[2]); i++) { 
    var inputs = [];
    var k = Math.floor(Math.random() * (unspent.length < 3 ? unspent.length : 3) + 1);
    for (var j = 0; j < k ; j++) {
        inputs = _.union(inputs, unspent.splice(Math.floor(Math.random() * unspent.length), 1));
        //inputs = inputs.concat(unspent.splice(Math.floor(Math.random() * unspent.length), 1));
    }

    if (!inputs.length)
        continue;
    var tx = ck.create_tx(inputs, 0);

    var tr = ck.createTx(tx);

    var _tx = [];
    _tx.push(ck.get_payload(tx));
    _tx.push(inputs);
    _tx.push(tx.getId());
    var temp =  h.runKernel(_tx);
    unspent = _.union(unspent, temp);
    tx_graph.push(tr);
   //   _.each(tx_graph, function(x){console.log(JSON.stringify(x));});
   // coins = coins.concat(temp);
   // coins = _.union(coins, inputs);
    
    //unspent = unspent.concat(h.runKernel(_tx));
}

console.timeEnd("running coinKernel...");


console.time("just sorting...");

h.topSort(tx_graph);

console.timeEnd("just sorting...");


//console.log(tx_graph);
//_.each(tx_graph, function(x){console.log(JSON.stringify(x));});
console.time("running coinkernel on graph...");

var result = h.runCoinKernelOnGraph(tx_graph);

console.timeEnd("running coinkernel on graph...");


//console.log(result);
//console.log(unspent);


    
   
