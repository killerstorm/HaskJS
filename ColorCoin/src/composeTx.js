const _           = require('lodash');
const bitcoin     = require('bitcoinjs-lib');
const buffertools = require('buffertools');
const Transaction = bitcoin.Transaction;

var dustThreshold = 546;

function createPayload (ins, outs, opid, outValues) {
    return '(' +
        JSON.stringify(_.range(ins)) + ', ' +
        JSON.stringify(_.range(outs)) + ', ' +
        outs + ') ' +
        opid.toString() + ' ' + JSON.stringify(outValues);
}
 
function selectCoins (unspentCoins, coinValueFn, neededSum) {
    var total = 0;
    var selected = [];
    
    for (var i = 0; i < unspentCoins.length; ++i) {
        var coinValue = coinValueFn(unspentCoins[i]);
        if (coinValue > 0) {
            total += coinValue;
            selected.push(unspentCoins[i]);
            if (total >= neededSum) break;
        }
    }
    
    if (total < neededSum)
        throw new Error ("Not enough coins!");
 
    return selected;
}
   
function composeColoredSendTx (unspentCoins, targets, changeAddress) {
    function coinValueFn (coin) {
        return coin.cv;
    }
 
    targets = _.clone(targets);
    var outValues =  _.map(targets, coinValueFn);
    var neededSum = _.sum(outValues);
    var coins     = selectCoins(unspentCoins, coinValueFn, neededSum);
    var inputSum  = _.sum(_.map(coins, coinValueFn));
    var change    = inputSum - neededSum;   
 
    if (change) {
        targets.push({address: changeAddress, cv: change.toString()});
        outValues.push(change);
    }
    return {inputs: coins, targets: targets};
}
 
function composeColoredIssueTx (targets) {
    return {inputs: [], targets: targets};
}
 
function composeBitcoinTx (coloredTx, unspentCoins, changeAddress) {
    var tx = new Transaction();
 
    unspentCoins = _.reject(unspentCoins, 'cv')
 
    var coloredTargets = coloredTx.targets;
    var coloredInputs  = coloredTx.inputs;
    var fee = 10000;
    var uncoloredNeeded   = coloredTargets.length * dustThreshold + fee;

    _.each(coloredTargets, function(target) {
        tx.addOutput(target.address, target.value);
    });
 
    _.each(coloredInputs, function(coin) {
        tx.addInput(coin.txid, coin.index);
        uncoloredNeeded -= coin.value;
    });
 
    var uncoloredSum = 0;
    var uncoloredInputs;
    if (uncoloredNeeded > 0) {
      uncoloredInputs = selectCoins(unspentCoins, function (coin) { return coin.value }, 
                                        uncoloredNeeded);
      uncoloredSum        = _.sum(uncoloredInputs, 'value');
      _.each(uncoloredInputs, function(coin) {  tx.addInput(coin.txid, coin.index); });
    }
 
    var change = uncoloredSum - uncoloredNeeded;
 
    if (change > 0) {
        tx.addOutput(changeAddress, change);
    }

    var payload = createPayload (
        coloredInputs === [] ? coloredInputs.length + uncoloredInputs.length : 0,
        change ? coloredTargets.length + 1 : coloredTargets.length,
        coloredInputs.length ? 0 : 1,
        _.pluck(coloredTargets, 'value').concat(change == 0 ? [] : [change])
    );
    
    tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0);
  
    return tx;  
}

module.exports = {
    composeColoredSendTx  : composeColoredSendTx,
    composeBitcoinTx      : composeBitcoinTx,
    composeColoredIssueTx : composeColoredIssueTx
}
