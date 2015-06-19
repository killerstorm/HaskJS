var  buffertools = require('buffertools');
var  bc = require('bitcoinjs-lib');
var  crypto = require('crypto');
var  Transaction = bc.Transaction;
var _ = require('lodash');

<<<<<<< HEAD
function get_inputs(transaction) {
    return transaction.ins.map(function (txin) {
        return [buffertools.reverse(txin.hash).toString('hex'), txin.index]  });
=======
function getInputs(transaction) {
    return transaction.ins.map(function (txin) {
        var txinhash = new Buffer(txin.hash);
        return [buffertools.reverse(txinhash).toString('hex'), txin.index]  });
>>>>>>> develop
}

function createKernelTx(t) {  //Tx for runCoinKernel
    var tx = [];
<<<<<<< HEAD
    tx.push(get_payload(t));
    tx.push(get_inputs(t));
=======
    tx.push(getPayload(t));
    tx.push(getInputs(t));
>>>>>>> develop
    tx.push(t.getId());
    tx.push(t.outs.length - 1);

    return tx;
}

function maybe_get_op_return(script) {
    if (script.chunks.length == 2 && script.chunks[0] == 106) {
         return script.chunks[1];
    } else { return null; }
}

<<<<<<< HEAD
function get_payload(transaction) {
=======
function getPayload(transaction) {
>>>>>>> develop
  for (var i = 0; i < transaction.outs.length; i++) {
      var op_return = maybe_get_op_return(transaction.outs[i].script);
      if (op_return) {
          return op_return.toString();
      }
  }
  return "";
}


<<<<<<< HEAD
module.exports = createKernelTx
=======
module.exports = {
    createKernelTx : createKernelTx,
    getPayload     : getPayload
}
>>>>>>> develop
