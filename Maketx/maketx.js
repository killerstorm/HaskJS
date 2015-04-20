#!/usr/bin/env node

var fs = require('fs');
var compose = require('./composeTx');
var co = require('./checkOptions');
var Getopt = require('node-getopt');


var opt = new Getopt([
    ['s' , 'send'                , 'make send tx.'],
    ['i' , 'issue'               , 'make issue tx.'],
    ['f' , 'input_file=ARG'      , 'load file'],
    ['o' , 'output_file=ARG'     , 'output file'],
    ['h' , 'help'                , 'display this help']
]).bindHelp();

var options = opt.parseSystem().options;
if (co.checkOptions(options)) {
    opt.showHelp();
    return;
}

var json = JSON.parse(fs.readFileSync(options.input_file));
var TX;

if (options.send)
    TX = compose.composeColoredSendTx (json.unspentColoredCoins, json.coloredTargets, json.coloredChangeAddress);
else
    TX = compose.composeColoredIssueTx (json.issueValue);

TX = compose.composeBitcoinTx(TX, json.context, json.UnspentCoins); 

TX = signTx(TX).toHex();

fs.writeFile(options.output_file, TX);
    

function signTx (tx) {
    throw new Error ('not implemented');
}
