var crypto = require('crypto');
var buffertools = require('buffertools');

function run_coin_kernel_on_graph(kernel_name, transactions) {
    var arr = [];
    for (var i = 0; i < transactions.length; i++) {
        var tx = [];
        json[0]    = _get_payload(transactions[i]);
        json[1]    = (get_inputs(transactions[i]));
        json[2]    = transactions[i].outs.length;
        json[3]    = transactions[i].getId();        
        arr[i] = tx;
    }
    return Haste[kernel_name](arr);
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
          return "([0], [1], 1) 0 [1]";
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




module.exports = {
    run_coin_kernel_on_graph : run_coin_kernel_on_graph,
    get_mux_shape            : get_mux_shape,
    get_payload              : get_payload,
    get_inputs               : get_inputs,
    _get_payload             : _get_payload
}
