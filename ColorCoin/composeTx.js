const tx          = require('./tx.js');
const bc          = require('bitcoinjs-lib');
const _           = require('lodash');
const Transaction = bc.Transaction;

const dustThreshold = 1000;


function composeColoredTx (unspentColoredCoins, targets, changeAddress, opid) {
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
    coins     = selectCoins(unspentColoredCoins, neededSum);
    inputSum  = _.sum(_.map(_.pluck(coins, 'cs'), parseInt));
    change    = inputSum - neededSum;
    

    if (change > 0) {
        newTx.addOutput(bc.scripts.pubKeyHashOutput(changeAddress), 0);
        outValues.push(change);
    }

    _.each(coins, function (_in) {
        newTx.addInput(_in.txid, _in.index);
    });

    _.each(targets, function(target) {
        newTx.addOutput(bc.scripts.pubKeyHashOutput(target.address), 0);
    });

    payload = createPayload (unspentColoredCoins.length, targets.length, opid, outValues);

    newTx.addOutput(bc.scripts.nullDataOutput(new Buffer(payload)), 0);

    return newTx;
}



function composeBitcoinTx (coloredTx, inputCoins, targets, unspentUncoloredCoins, allCoins) {
    var uncoloredNeeded;
    





    
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
}

    
