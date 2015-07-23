var composetx  = require('./composeTx.js')
var kerneltx   = require('../lib/kerneltx.js')
var haste      = require('../lib/Haste.js').getHaste()
var _          = require('lodash')

var createKernelTx = kerneltx.createKernelTx
var _runKernel = haste.runKernel
var _runCoinKernelOnGraph = haste.runCoinKernelOnGraph

/**
 * Kernel.
 * @constructor
 */
function Kernel (kernelName, sim) {
  this.name = kernelName
  this.simulation = sim
}

/**
 * Process transaction
 * @param {string} tx
 * @return {[Object]} 
 */
Kernel.prototype.processTx = function (tx, coloredOutsNumber, color) {
  var coins = runCoinKernelOnGraph(tx, this.simulation.transactions)
  for (var i = 0; i < coloredOutsNumber; i++)
    coins[i].cv = new ColorValue (color, coins[i].value)

  return coins
}
    
/**
 * run kernel
 * @param {string} tx
 * @param {[Object]} unputCoins
 * @return {[Object]}
 */
function runKernel (tx, inputCoins) {

  var optx = createKernelTx (tx) 
  var coins = _.map(_runKernel(JSON.stringify(optx)), JSON.parse)

  return coins
}

/**
 * run kernel on graph
 * @param {string} tx
 * @param {[Transaction]} txs
 * @return {[Object]}
 */
function runCoinKernelOnGraph (tx, txs) {
  var transactions = _.chain(txs)
                     .map(createKernelTx)
                     .map(JSON.stringify)
                     .value()
  
  var optx = JSON.stringify(createKernelTx(tx))
  
  var coins = _.map(_runCoinKernelOnGraph(transactions, optx), JSON.parse)

  return coins
}


/**
 * Color
 * @constructor
 * @param {string} colorID
 * @param {Kernel} kernel
 * @param {string} name
 */
function Color (kernel, colorID, name) {
  this.name    = name
  this.kernel  = kernel
  this.colorID = colorID
}

Color.prototype.getName = function () {
  return this.name
}

/**
 * Get kernel
 * @return {Kernel}
 */
  Color.prototype.getKernel = function () {
  return this.kernel
}

/**
 * ColorValue
 * @constructor
 * @param {number} value
 */
function ColorValue (color, value) {  
  this.color = color
  this.value = value
}

/**
 * Get color
 * @return {Color}
 */
ColorValue.prototype.getColor = function () {
  return this.color
}

/**
 * getValue
 * @return {number}
 */
ColorValue.prototype.getValue = function() {
  return this.value
}

module.exports = {
  Kernel      : Kernel,
  Color       : Color,
  ColorValue  : ColorValue
}