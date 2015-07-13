var composetx = require('./composeTx.js');
var kerneltx  = require('../lib/kerneltx.js');
var haste     = require('../lib/Haste.js').getHaste();
var _         = require('lodash');


var createKernelTx = kerneltx.createKernelTx
var runKernel = haste.runKernel
var runCoinKernelOnGraph = haste.runCoinKernelOnGraph

function Kernel (simulation) {
  this.simulation = simulation;
}

Kernel.prototype.runKernel = function (tx) {

  var optx = createKernelTx (tx) 
  
  var coins = _.map(runKernel(JSON.stringify(optx)), JSON.parse)

  return coins;
}

Kernel.prototype.runKernelOnGraph = function (tx) {
  var transactions = _.chain(this.simulation.transactions)
                     .map(createKernelTx)
                     .map(JSON.stringify)
                     .value()

  console.log(transactions)
  var optx = JSON.stringify(createKernelTx(tx))
  
  var coins = _.map(runCoinKernelOnGraph(transactions, optx), JSON.parse)

  return coins
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
