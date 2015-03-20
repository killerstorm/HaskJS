var ckernel = require ('./main.js');
const bc = require('bitcoinjs-lib');
const bt = require('buffertools');
var Transaction = bc.Transaction;
var crypto = require('crypto');
var _ = require('underscore');

var h = ckernel.getHaste()


function maybe_get_op_return(script) {
    if (script.chunks.length == 2 && script.chunks[0] == 106) {
         return script.chunks[1];
    } else { return null; }
}

function get_payload(transaction) {
  for (var i = 0; i < transaction.outs.length; i++) {
      var op_return = maybe_get_op_return(transaction.outs[i].script);
      if (op_return) {
          return op_return.toString();
      }
  }
  return "";
}


function get_inputs(transaction) {
    var inputs = [];
    for (var i = 0; i < transaction.ins.length; i++) {
        var temp = [];
        temp[0] = (transaction.ins[i].hash.toString('hex'));
        temp[1] = (transaction.ins[i].index);
        inputs[i] = temp;
    }
    return inputs;
}

function createTx(t) {  //Tx for runCoinKernel
    var tx = [];
    tx.push(get_payload(t));
    tx.push(get_inputs(t));
    tx.push(t.getId());
    tx.push(t.outs.length);

    return tx;
}


function get_random_sums(n) {
    var k = n < 3 ? n : Math.floor(Math.random() * 3 + 1);
    var out_sums = [];
    for (var i = 1; i < k; i++) {
        var sum = Math.floor(Math.random() * (n / 2) + 1);
        out_sums.push(sum);
        n -= sum;
    }
    out_sums.push(n);
    return out_sums;
}
           

function create_tx(inputs, opid) {
    var tx = new Transaction();
    var amount = 0;

    for (var i = 0; i < inputs.length; i++) {
        tx.addInput(inputs[i][0][0], inputs[i][0][1]);
        amount += inputs[i][1];
    }

    var outsums = get_random_sums(amount);
    
    for (var i = 0; i < outsums.length; i++) {
        tx.addOutput(bc.scripts.pubKeyHashOutput(crypto.randomBytes(20)), outsums[i]);
    }

    var payload = '(' + JSON.stringify(_.range(inputs.length))  + ',' +
        JSON.stringify(_.range(outsums.length)) + ',' +
        outsums.length + ')' + opid.toString() + JSON.stringify(outsums);

    tx.addOutput(bc.scripts.nullDataOutput(new Buffer(payload)), 0);
    return tx;
}




var unspent = [];


var test_payload = new Buffer("([], [0], 1) 1 [50]");
var test_tx = new Transaction();
test_tx.addOutput(bc.scripts.pubKeyHashOutput(crypto.randomBytes(20)), 0);
test_tx.addOutput(bc.scripts.nullDataOutput(new Buffer(test_payload)), 0);


var tx = [];
tx.push(createTx(test_tx));

unspent = unspent.concat(h.runCoinKernelOnGraph(tx));
//console.log(unspent);


for (var i = 0; i < 10000; i++) {
    var inputs = [];
    var k = Math.floor(Math.random() * (unspent.length < 3 ? unspent.length : 3) + 1);
    for (var j = 0; j < k ; j++) {
        inputs = inputs.concat(unspent.splice(Math.floor(Math.random() * unspent.length), 1));
    }
    if (!inputs.length)
        continue;
    var tx = create_tx(inputs, 0);
    var _tx = [];
    _tx.push(get_payload(tx));
    _tx.push(inputs);
    _tx.push(tx.getId());
    unspent = unspent.concat(h.runKernel(_tx));
}

console.log(unspent);


    
   
