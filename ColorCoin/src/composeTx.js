const bitcoin     = require('bitcoinjs-lib');
const _           = require('lodash');
const Transaction = bitcoin.Transaction;

const dustThreshold = 1000;


function composeColoredTx (unspentCoins, targets, changeAddress, opid) {
    var newTx;
    var neededSum;
    var change;
    var inputSum;
    var payload;
    var outValues;
    var coins;

    newTx     = new Transaction();    
    outValues = _.pluck(targets, 'value');
    neededSum = _.sum(outValues);
    coins     = unspentCoins.selectColored(neededSum);
    inputSum  = _.sum(_.map(_.pluck(coins, 'cs'), parseInt));
    change    = inputSum - neededSum;
    

    if (change > 0) {
        newTx.addOutput(bitcoin.scripts.pubKeyHashOutput(changeAddress), 0);
        outValues.push(change);
    }

    _.each(coins, function (_in) {
        newTx.addInput(_in.txid, _in.index);
    });

    _.each(targets, function(target) {
        newTx.addOutput(bitcoin.scripts.pubKeyHashOutput(target.address), 0);
    });

    payload = createPayload (unspentColoredCoins.length, targets.length, opid, outValues);

    newTx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0);

    return newTx;
}



function composeBitcoinTx (coloredTx, targets, unspentUncoloredCoins, allUnspentCoins, inputColoredCoins, ) {
    var uncoloredNeeded;
   
    
}
    
function createPayload (ins, outs, opid, outsums) {
    return '(' + JSON.stringify(_.range(ins)) + ', ' +
        JSON.stringify(_.range(outs)) + ', ' +
        outs.toString() + ') ' +
        opid.toString() + ' ' + JSON.stringify(outsums);
}


module.exports = {
    composeColoredTx : composeColoredTx
    composeBitconTx  : composeBitcoinTx
}

    
