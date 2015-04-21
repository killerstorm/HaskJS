const _           = require('lodash');
const bitcoin     = require('bitcoinjs-lib');
const buffertools = require('buffertools');
const Transaction = bitcoin.Transaction;


function createPayload (ins, outs, opid, outValues) {
    return '(' + JSON.stringify(_.range(ins)) + ', ' +
        JSON.stringify(_.range(outs)) + ', ' +
        outs.toString() + ') ' +
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
    var payload = createPayload(coins, targets, 0, outValues);
    return {inputs: coins, targets: targets, payload: payload};
}
 
function composeColoredIssueTx (value) {
  var payload = createPayload([], [0], 1, [value]);
  return {inputs: [], targets: [], payload: payload};
}
 
 
function composeBitcoinTx (coloredTx, context, unspentCoins) {
    var tx = new Transaction();
 
    unspentCoins = _.reject(unspentCoins, 'cv')
 
    var coloredTargets = coloredTx.targets;
    var coloredInputs  = coloredTx.inputs;
    var fee = 10000;
    var uncoloredNeeded   = coloredTargets.length * context.dustThreshold + fee;
 
    _.each(coloredTargets, function(target) {
        tx.addOutput(bictoin.scripts.pubKeyHashOutput(
            new Buffer (target.address)), context.dustThreshold);
    });
 
    _.each(coloredInputs, function(coin) {
        tx.addInput(coin.txid, coin.index);
        uncoloredNeeded -= coin.value;
    });
 
    var uncoloredSum = 0;
    if (uncoloredNeeded > 0) {
      var uncoloredInputs = selectCoins(unspentCoins, function (coin) { return coin.value }, 
                                        uncoloredNeeded);
      uncoloredSum        = _.sum(_.pluck(uncoloredInputs, 'value'));
      _.each(uncoloredInputs, function(coin) {  tx.addInput(coin.txid, coin.index); });
    }
 
    var change = uncoloredSum - uncoloredNeeded;
 
    if (change > 0) {
        tx.addOutput(bictoin.scripts.pubKeyHashOutput(
            new Buffer (context.changeAddress)), change);
    }
 
    tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(coloredTx.payload)), 0);
  
    return tx;  
}

module.exports = {
    composeColoredSendTx  : composeColoredSendTx,
    composeBitcoinTx      : composeBitcoinTx,
    composeColoredIssueTx : composeColoredIssueTx
}
