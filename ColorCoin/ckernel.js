function init() {
}
    
function run_coin_kernel_on_graph(kernel_name, transactions) {
    var arr = [];
    for (var i = 0; i < transactions.length; i++) {
        var json = {};
        json.pload    = get_payload(transactions[i]);
        json.ins      = get_inputs(transactions[i]);
        json.outs = transactions[i].outs.length;
        json.txid     = i;        
        arr[i] = JSON.stringify(json);
    }
    return Haste[kernel_name](arr);
}

function get_mux_shape(kernel_name, payload) {
    
}

function maybe_get_op_return(script) {
  if (script.chunks.length == 2 && script.chunks[0] == 106) {
    return script.chunks[1];
  } else { return null; }
}
 
function get_payload(transaction) {
  for (var i = 0; i < transaction.outs.length; i++) {
      var op_return = maybe_get_op_return(transaction.outs[i].script);
      if (op_return) return op_return.toString('hex');
  }
  return "";
}

function get_inputs(transaction) {
    var inputs = {};
    for (var i = 0; i < transaction.ins.length; i++) {
        var temp = {};
        temp.hashHex = transaction.ins[i].hash.toString('hex');
        temp.index   = transaction.ins[i].index;
        inputs[i] = temp;
    }
    return inputs;
}

function get_txid(transaction) {
    
}

exports.run_coin_kernel_on_graph = run_coin_kernel_on_graph;
exports.get_mux_shape            = get_mux_shape;
exports.get_payload              = get_payload;
exports.get_inputs               = get_inputs;
