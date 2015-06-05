var _ = require('lodash');
var bitcoin = require('bitcoinjs-lib');
var Kernel = require('./Kernel.js');

function Simulation() {
    this.kernel = new Kernel();
    this.transactions = [];
    this.wallets = {};
    this.coins = [];
    this.unspentCoins = [];
}
 
Simulation.prototype.wallet = function (name) {
    this.wallets[name] = new Wallet(this);
    return this.wallets[name];
}
 
Simulation.addTx = function (tx) {
    this.transactions.push(tx);
}

Simulation.prototype.getUnspentCoins = function (addr) {
    var unspent = [];  
    _.map(this.transactions, function(tx) {
        var index = 0;
        _.map(tx.outs, function(out) {
            if (bitcoin.Address.fromOutputScript(out.script).toString() == addr) {
                _.map(this.coins, function(c) {
                    id = tx.getId();
                    if (c.txid == id && c.index == index && c.value == out.value) {
                        unspent.push(c);
                    }
                });
                index++;
            }
        });
    });

    _.difference(this.unspentCoins, unspent);
    return unspent;                   
}

function Wallet(simulation) {
    this.simulation = simulation;
    var key = bitcoin.ECKey.makeRandom();
    this.privkey = key;
    this.pubkey  = key.pub;
    this.address = key.pub.getAddress().toString();
    this.coins = [];
}
 
Wallet.prototype.issueCoin = function (kernel, value) {
    var tx = this.simulation.composeIssueTx(kernel, value, this.getUnspentCoins());
    var signedTx = this.signTx(tx);
    this.simulation.addTx(signedTx);
}


Wallet.prototype.send = function (value, target) {
    var tx = this.simulation.composeSendTx();
}

Wallet.prototype.getUnspentCoins = function () {
    this.coins = this.coins.concat(this.simulation.getUnspentCoins(this.address));
    return this.coins;
}
 
Wallet.prototype.signTx = function (tx) {
    for (var i = 0; i < tx.ins.length; tx.sign(i, this.privkey), i++);  
}

Wallet.prototype.getAddress = function() {
    return this.address;
}

module.exports = Simulation
