#!/usr/bin/env node

var fs = require('fs');
var compose = require('./composeTx');
//var Getopt = require('node-getopt');

var opt = require('node-getopt').create([
    ['s' , 'send'                , 'make send tx.'],
    ['i' , 'issue'               , 'make issue tx.'],
    ['f' , 'input_file=ARG'      , 'load file'],
    ['o' , 'output_file=ARG'     , 'output file'],
    ['h' , 'help'                , 'display this help']
])
.bindHelp()
.parseSystem();

var options = opt.options;

if (!((options.send || options.issue) && !(options.send && options.issue))) {
    throw new Error ('One operational option must be specified');
}

if (!options.input_file) {
    throw new Error ('Input file not specified!');
}

if (!fs.existsSync(options.input_file)) {
    throw new Error ('File does not exist.');
}

var json = JSON.parse(fs.readFileSync(options.input_file));
var coloredTx;
var bitoinTx;

if (options.send)
    coloredTx = composeColoredSendTx (json.unspentColoredCoins, json.coloredTargets, json.coloredChangeAddress);
else
    coloredTx = composeColoredIssueTx (json.issueValue);
