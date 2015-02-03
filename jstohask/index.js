const bc = require('bitcoinjs-lib');
var Transaction = bc.Transaction;

var tx = Transaction.fromHex('010000000189632848f99722915727c5c75da8db2dbf194342a0429828f66ff88fab2af7d600000000fd1b0100483045022100e5be20d440b2bbbc886161f9095fa6d0bca749a4e41d30064f30eb97adc7a1f5022061af132890d8e4e90fedff5e9365aeeb77021afd8ef1d5c114d575512e9a130a0147304402205054e38e9d7b5c10481b6b4991fde5704cd94d49e344406e3c2ce4d18a43bf8e022051d7ba8479865b53a48bee0cce86e89a25633af5b2918aa276859489e232f51c014c8752410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b84104c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee51ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a52aeffffffff0101000000000000001976a914751e76e8199196d454941c45d1b3a323f1433bd688ac00000000');

var json = {
    version: tx.version,
    locktime: tx.locktime,
    ins: tx.ins,
    outs: tx.outs,
    addInput: tx.addInput,
    hashForSignature: tx.hashForSignature,
    getHash: tx.getHash,
    getId: tx.getID,
};

var json2;

function parse() {
    json2 = Haste.parse(JSON.stringify(json));
    console.log(json2);
}


