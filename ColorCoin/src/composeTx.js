const bitcoin     = require('bitcoinjs-lib');
const _           = require('lodash');
const Transaction = bitcoin.Transaction;
const buffertools = require('buffertools');

const dustThreshold = 5460;


function composeColoredTx (unspentCoins, targets, changeAddress, colorId) {
    var neededSum;
    var change;
    var inputSum;
    var coins;
    
    neededSum = _.sum(_.pluck(targets, 'value'));
    coins     = selectCoins(unspentCoins, needeedSum);
    inputSum  = _.sum(_.map(_.pluck(coins, 'cs'), parseInt));
    change    = inputSum - neededSum;   

    if (change) {
        targets.push({address: changeAddress, value: change});
    }

    return {inputs: coins, targets: targets};
}



function composeBitcoinTx (coloredTx, targets, changeAddress, unspentCoins, opid = 0) {
    var tx;
    var uncoloredNeeded;
    var uncoloredSum;
    var change;
    var coloredTargets;
    var coloredInputs;
    var outValues;
    var payload;
    var fee;

    tx = new Transaction();

    coloredTargets = coloredTx.targets;
    coloredInputs  = coloredTx.inputs;
    uncoloredNeeded   = _.sum(_.pluck(targets, 'value'));
    outValues = [];


    _.each(coloredTargets.append(uncoloredTargets), function(target) {
        var value = target.value;
        if (target.value < dustThreshold) {
            value = dustThreshold;
            uncoloredNeeded += (dustThreshold - target.value);
        }
        tx.addOutput(bictoin.scripts.pubKeyHashOutput(
            new Buffer (target.address)), value);
        outValues.push(value);
    });


    uncoloredInputs     = selectCoins(unspentCoins, uncoloredNeeded);
    uncoloredSum        = _.sum(_.map(_.pluck(uncoloredInputs, 'cs'), parseInt));
    
    _.each(coloredInputs.append(uncoloredInputs), function(coin) {
        tx.addInput(coin.txid, coin.index);
    });

    fee = estimateFee (tx);

    change = uncoloredSum - fee - uncoloredNeeded;

    if (change) {
        tx.addOutput(bictoin.scripts.pubKeyHashOutput(
            new Buffer (changeAddress)), change);
        outValues.push(change); 
    }

    payload = createPayload(coloredInputs.length + uncoloredInputs.length, coloredTargets.length + uncoloredTargts.length, opid, outValues);

    tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0);
  
    return tx;  
}

function composeTx() {
    throw new Error ("composeTx not implemented!");
}


function estimateFee() {
    throw new Error ("estimateFee not implemented!");
}

function selectCoins (unspentCoins, neededSum) {
    var total = 0;
    var unspent = _.takeWhile(unspentCoins, function(n) {
        var value = parseInt(n.cs) == NaN ? 0 : parseInt(n.cs);
        return total >= neededSum ? false : (total += value) && true;
    });

    if (total < neededSum)
        throw new Error ("Not enough coins!");

    unspentCoins = _.difference(unspentCoins, unspent);
    unspent = _.filter(unspent, function(n) {return parseInt(n.cs) != NaN;});
    return unspent;        
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

    
