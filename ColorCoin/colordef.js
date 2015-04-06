const tx          = require('./tx.js');
const bc          = require('bitcoinjs-lib');
const _           = require('lodash');
const Transaction = bc.Transaction;


function composeColoredTx (unspentColoredCoins, targets, changeAddress, opid) {
    var newTx;
    var neededSum;
    var change;
    var inputSum;
    var payload;
    var outValues;
    var inValues;

    // var sortedByColors = sortTargetsByColor(targets);
    // var uncoloredNedeedSum;
    
    newTx     = new Transaction();
    inValues  = _.map(_.pluck(unspentColoredCoins, 'cs'), parseInt);
    outValues = _.pluck(targets, 'value');
    neededSum = _.sum(outValues);
    inputSum  = _.sum(inValues);
    change    = inputSum - neededSum;
    

    if (change > 0) {
        newTx.addOutput(bc.scripts.pubKeyHashOutput(changeAddress), 0);
        outValues.push(change);
    }

    _.each(unspentColoredCoins, function (_in) {
        newTx.addInput(_in.txid, _in.index)});

    _.each(targets, function(target) {
        newTx.addOutput(bc.scripts.pubKeyHashOutput(target.address), 0);
    });

    payload = createPayload (unspentColoredCoins.length, targets.length, opid, outValues);

    newTx.addOutput(bc.scripts.nullDataOutput(new Buffer(payload)), 0);
    return newTx;
}



function composeBitcoinTx () {



    
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

    
