var composetx = require('./composeTx.js');
var kerneltx  = require('../src/kerneltx.js');
var haste     = require('../main.js').getHaste();
var _         = require('lodash');


function Kernel (simulation) {
  this.simulation = simulation;
}

Kernel.prototype.runKernel = function (tx) {

  var optx = kerneltx.createKernelTx (tx) 
  
  var coins = _.map(haste.runKernel(JSON.stringify(optx)), JSON.parse);

  return coins;
}
        
function Color () {
  throw new Error ("Color not implemented");
    //TODO
}

function ColorValue (colorId, value) {
  this.colorId = colorId;
  this.value    = value;
} 

module.exports = {
  Kernel      : Kernel,
  Color       : Color,
  ColorValue  : ColorValue
}
