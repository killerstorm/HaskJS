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
    var outSums;

    // var sortedByColors = sortTargetsByColor(targets);
    // var uncoloredNedeedSum;

    newTx = new Transaction();
    neededSum = _.reduce(targets, function(sum, n) {return sum + n[1];});
    inputSum  = _.reduce(unspentColoredCoins, function (sum, n) {return sum + n[1];});
    outSums   = _.map(targets, function(x) {return x[1];});
    change = inputSum - neededSum;

    if (change > 0)
        newTx.addOutput(bc.scripts.pubKeyHashOutput(changeAddress), change);

    _.each(_.map(unspentColoredCoins, getInput), function (_in) {
        newTx.addInput(_in[0], _in[1])});

    _.each(targets, function(x) {
        newTx.addOutput(bc.scripts.pubKeyHashOutput(x[0]), x[1]);
    });

    payload = createPayload (unspentColoredCoins.length, targets.length, opid, outSums);

    newTx.addOutput(bc.scripts.nullDataOutput(new Buffer(payload)), 0);
    return newTx;
}



function composeBitcoinTx () {
}


function createPayload (ins, outs, opid, outsums) {
    return '(' + JSON.stringify(range(ins)) + ', ' +
        JSON.stringify(range(outs)) + ', ' +
        outs.toString() + ') ' +
        opid.toString() + ' ' + JSON.stringify(outsums);
}

function getInput(coins) {
    return coins[0];
}

               


function range(n) {
    var r = [];
    for (var i = 0; i < n; r.push(i), i++);
    return r;
}


function sortTargetsByColor(targets) {
}

module.exports = {
    composeColoredTx : composeColoredTx
}

    
