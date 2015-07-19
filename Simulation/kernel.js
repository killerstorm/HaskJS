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
function Kernel (simulation, kernelName) {
  switch (kernelName) {
    case 'toy' :
      this.runKernel = runKernel
      this.runCoinKernelOnGraph = runCoinKernelOnGraph
      break
    default:
      throw new Error("Kernel does not exist!")
  }
}


/**
 * run kernel
 * @param {string} tx
 */
function runKernel (tx) {

  var optx = createKernelTx (tx) 
  var coins = _.map(_runKernel(JSON.stringify(optx)), JSON.parse)

  return coins
}

/**
 * run kernel on graph
 * @param {string} tx
 * @param {[Transaction]} txs
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
 */
function Color (kernel, colorID) {
  this.kernel  = kernel
  this.colorID = colorID
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

module.exports = {
  Kernel      : Kernel,
  Color       : Color,
  ColorValue  : ColorValue
}