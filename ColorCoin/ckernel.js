var  buffertools = require('buffertools');
var  bc = require('bitcoinjs-lib');
var  crypto = require('crypto');
var  Transaction = bc.Transaction;

function get_inputs(transaction) {
    return transaction.ins.map(function (txin) {
        return [buffertools.reverse(txin.hash).toString('hex'), txin.index]  });
}

function createTx(t) {  //Tx for runCoinKernel
    var tx = [];
    tx.push(get_payload(t));
    tx.push(get_inputs(t));
    tx.push(t.getId());
    tx.push(t.outs.length - 1);

    return tx;
}

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

    var payload = '(' + JSON.stringify(range(inputs.length))  + ', ' +
        JSON.stringify(range(outsums.length)) + ', ' +
        outsums.length + ') '+
        opid.toString() + ' ' + JSON.stringify(outsums);

    tx.addOutput(bc.scripts.nullDataOutput(new Buffer(payload)), 0);
    return tx;
}


function range(n) {
    var r = [];
    for (var i = 0; i < n; r.push(i), i++);
    return r;
}


module.exports = {
    createTx                 : createTx,
    create_tx                : create_tx,
    get_payload              : get_payload,
    get_inputs               : get_inputs
}
