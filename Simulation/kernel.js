var composetx = require('../ColorCoin/src/composeTx.js');
var ckernel = require('../ColorCoin/src/ckernel.js');
var haste = require('../ColorCoin/main.js').getHaste();

function Kernel() {
    
}

Kernel.prototype.run = function(tx) {
    var _tx = ckernel.createTx(tx);
    var coins = haste.runKernel(_tx);
    return coins;
}

Kernel.prototype.composeIssueTx = function (value, targets) {
    var tx = composetx.composeColoredIssueTx (value, targets);
    return tx;
}

Kernel.prototype.composeSendTx = function (unspent, targets, changeAddress) {
    var tx = composetx.composeColoredSendTx (unspent, targets, changeAddress);
    return tx;
}

Kernel.prototype.composeBitcoinTx = function (tx, unspent) {
    var tx = composetx.composeBitcoinTx (tx, unspent);
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
