var composetx = require('./composeTx.js');
var createTx = require('../ColorCoin/src/kerneltx.js');
var haste = require('../ColorCoin/main.js').getHaste();
var _ = require('lodash');


function Kernel(simulation) {
    this.simulation = simulation;
}

Kernel.prototype.runKernel = function(tx) {
    var txGraph = _.map(transactions, createTx);
    var coins = _.map(haste.runCoinKernelOnGraph(txGraph), JSON.parse);
    return coins;
}

Kernel.prototype.composeIssueTx = function (targets) {
    var tx = composetx.composeColoredIssueTx (targets);
    return tx;
}

Kernel.prototype.composeSendTx = function (unspent, targets, changeAddress) {
    var tx = composetx.composeColoredSendTx (unspent, targets, changeAddress);
    return tx;
}

Kernel.prototype.composeBitcoinTx = function (tx, uncoloredWallet) {
    var tx = composetx.composeBitcoinTx (tx, uncoloredWallet);
    return tx;
}
        
function Color() {
    throw new Error ("Color not implemented");
    //TODO
}


function ColorValue(colorId, value) {
    this.colorId = colorId;
    this.valu    = value;
}

module.exports = {
    Kernel     : Kernel,
    Color      : Color,
    ColorValue : ColorValue
}
