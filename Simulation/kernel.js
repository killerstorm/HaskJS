var composetx = require('../ColorCoin/src/composeTx.js');
var ckernel = require('../ColorCoin/src/ckernel.js');
var haste = require('../ColorCoin/main.js').getHaste();
var _ = require('lodash');


function Kernel() {
    
}

Kernel.prototype.run = function(transactions) {
    var txGraph = _.map(transactions, ckernel.createTx);
    var coins = haste.runCoinKernelOnGraph(txGraph);
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

function ColorValue() {
    throw new Error ("ColorValue not implemented");
    //TODO
}

module.exports = {
    Kernel     : Kernel,
    Color      : Color,
    ColorValue : ColorValue
}
