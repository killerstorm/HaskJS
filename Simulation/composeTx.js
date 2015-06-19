<<<<<<< HEAD
=======

>>>>>>> develop
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
    
    var selected = _.takeWhile(unspentCoins, function (n) { return (total += coinValueFn(n)) >= neededSum });

    if (total < neededSum)
        throw new Error ("Not enough coins!");
 
    return selected;
}
   
function composeColoredSendTx (wallet, targets, changeAddress) {
    function coinValueFn (coin) {
        return coin.value;
    }

    var unspentCoins = wallet.getUnspentCoins();

    targets = _.clone(targets);
    var neededSum = _.sum(targets, 'value');
    var coins     = selectCoins(unspentCoins, coinValueFn, neededSum);
    var inputSum  = _.sum(coins, 'value');
    var change    = inputSum - neededSum;   


    wallet.coins = _.difference(wallet.coins, coins);
<<<<<<< HEAD
    if (change) {
=======
    if (change > 0) {
>>>>>>> develop
        targets.push({address: changeAddress, value: change});
    }
    
    return {inputs: coins, targets: targets};
}
 
function composeColoredIssueTx (targets) {
    return {inputs: [], targets: targets};
}
 
function composeBitcoinTx (coloredTx, uncoloredWallet) {
    var tx = new Transaction();

    var unspentCoins = uncoloredWallet.getUnspentCoins();
    var changeAddress = uncoloredWallet.getAddress();

<<<<<<< HEAD
    var coins = [];
=======
>>>>>>> develop
    var index = 0;
    unspentCoins = _.reject(unspentCoins, 'cv')
 
    var coloredTargets = coloredTx.targets;
    var coloredInputs  = coloredTx.inputs;
    var fee = 10000;
    var uncoloredNeeded   = coloredTargets.length * dustThreshold + fee;

    _.each(coloredTargets, function(target) {
        tx.addOutput(target.address, target.value);
<<<<<<< HEAD
        coins.push({"index" : index, "value" : target.value, "coinstate" : target.value.toString()});
=======
>>>>>>> develop
        index++;
    });
 
    _.each(coloredInputs, function(coin) {
        tx.addInput(coin.txid, coin.index);
        uncoloredNeeded -= coin.value;
    });
<<<<<<< HEAD
 
    var uncoloredSum = 0;
    var uncoloredInputs;
=======

    var uncoloredSum = 0;
    var uncoloredInputs = [];
>>>>>>> develop
    var change = 0;
    
    if (uncoloredNeeded > 0) {
        uncoloredInputs = selectCoins(unspentCoins, function (coin) { return coin.value }, 
                                      uncoloredNeeded);
        uncoloredSum        = _.sum(uncoloredInputs, 'value');
        _.each(uncoloredInputs, function(coin) {  tx.addInput(coin.txid, coin.index); });
          change = uncoloredSum - uncoloredNeeded;
  
        if (change > 0) {
            tx.addOutput(changeAddress, change);
<<<<<<< HEAD
            coins.push({"index" : index, "value" : change, "coinstate" : change.toString()});
        }
    }

=======
        }
    }

    var outValues = _.pluck(coloredTargets, 'value').concat(change == 0 ? [] : [change])

>>>>>>> develop
    var payload = createPayload (
        coloredInputs === [] ? coloredInputs.length + uncoloredInputs.length : 0,
        change ? coloredTargets.length + 1 : coloredTargets.length,
        coloredInputs.length ? 0 : 1,
<<<<<<< HEAD
        _.pluck(coloredTargets, 'value').concat(change == 0 ? [] : [change])
=======
        outValues
>>>>>>> develop
    );
    
    tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0);
    
    uncoloredWallet.coins = _.difference(uncoloredWallet.coins, uncoloredInputs);
<<<<<<< HEAD
    
    return {'tx' : tx, 'coins' : coins};  
=======

    return {'tx' : tx, 'outValues' : outValues, 'inputs' : coloredInputs.concat(uncoloredInputs)};  
>>>>>>> develop
}

module.exports = {
    composeColoredSendTx  : composeColoredSendTx,
    composeBitcoinTx      : composeBitcoinTx,
    composeColoredIssueTx : composeColoredIssueTx
}
