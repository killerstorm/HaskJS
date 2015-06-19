var composetx = require('./composeTx.js');
<<<<<<< HEAD
var createTx = require('../ColorCoin/src/kerneltx.js');
=======
var kerneltx = require('../ColorCoin/src/kerneltx.js');
>>>>>>> develop
var haste = require('../ColorCoin/main.js').getHaste();
var _ = require('lodash');


function Kernel(simulation) {
    this.simulation = simulation;
}

<<<<<<< HEAD
Kernel.prototype.runKernel = function(tx) {
    var txGraph = _.map(transactions, createTx);
    var coins = _.map(haste.runCoinKernelOnGraph(txGraph), JSON.parse);
=======
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
>>>>>>> develop
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
<<<<<<< HEAD
    this.value   = value;
=======
    this.valu    = value;
>>>>>>> develop
}

module.exports = {
    Kernel     : Kernel,
    Color      : Color,
    ColorValue : ColorValue
}
