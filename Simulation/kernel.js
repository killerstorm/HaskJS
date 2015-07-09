var composetx = require('./composeTx.js');
var kerneltx  = require('../src/kerneltx.js');
var haste     = require('../main.js').getHaste();
var _         = require('lodash');


function Kernel(simulation) {
  this.simulation = simulation;
}

Kernel.prototype.runKernel = function(tx, ins, outs) {
  var ktx = [kerneltx.getPayload(tx), tx.getId()];
  ins = _.map(ins, function(n) {
    var t = [];
    t[0] = [];
    t[0].push(n.txid);
    t[0].push(n.index);
    t.push(n.value);
    return t;
  });

  var coins = _.map(haste.runKernel(ktx, ins, outs), JSON.parse);
  return coins;
}
        
function Color() {
  throw new Error ("Color not implemented");
    //TODO
}

function ColorValue(colorId, value) {
  this.colorId = colorId;
  this.value    = value;
} 

module.exports = {
  Kernel      : Kernel,
  Color       : Color,
  ColorValue  : ColorValue
}
