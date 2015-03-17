var crypto = require('crypto');
var buffertools = require('buffertools');

function run_coin_kernel_on_graph(kernel_name, transactions) {
    var arr = [];
    for (var i = 0; i < transactions.length; i++) {
        var tx = [];
        tx.push(_get_payload(transactions[i]));
        tx.push(get_inputs(transactions[i]));
        tx.push(transactions[i].getId());
        tx.push(transactions[i].outs.length);
        arr[i] = tx;
    }
    console.log(Haste);
    return Haste[kernel_name](arr);
}

function createTx(t) {
    var tx = [];
    tx.push(_get_payload(t));
    tx.push(get_inputs(t));
    tx.push(t.getId());
    tx.push(t.outs.length);

    return tx;
}



function get_mux_shape(payload) {
    return Haste["getMuxShape"](payload);   
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
          return buffertools.reverse(op_return).toString("hex");
      }
  }
  return "";
}

function _get_payload(transaction) {
  for (var i = 0; i < transaction.outs.length; i++) {
      var op_return = maybe_get_op_return(transaction.outs[i].script);
      if (op_return) {
          return "([], [0, 1, 2, 3], 4) 1 [3, 7, 8, 9]";
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

function run_coin_kernel_on_issue(issuetx) {
    return Haste["runCoinKernelOnIssue"](issuetx);
}

function getHaste() {
    return Haste
}

module.exports = {
    getHaste                 : getHaste,
    createTx                 : createTx,
    run_coin_kernel_on_graph : run_coin_kernel_on_graph,
    run_coin_kernel_on_issue : run_coin_kernel_on_issue,
    get_mux_shape            : get_mux_shape,
    get_payload              : get_payload,
    get_inputs               : get_inputs,
    _get_payload             : _get_payload
}
