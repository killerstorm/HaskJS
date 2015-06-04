var _ = require('lodash');
var bitcoin = require('bitcoinjs-lib');

function Simulation() {
    this.transactions = [];
    this.wallets = {};
    this.coins = [];
}
 
Simulation.prototype.wallet = function (name) {
    this.wallets[name] = new Wallet(this);
    return this.wallets[name];
}
 
Simulation.prototype.addTx = function (tx) {
    this.transactions.push(tx);
}

Simulation.prototype.getUnspentCoins(addr) {
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
                
    return unspent;                   
}

function Wallet(simulation) {
    this.simulation = simulation;
    var key = bitcoin.ECKey.makeRandom();
    this.privkey = key;
    this.pubkey  = key.pub;
    this.address = key.pub.getAddress().toString();
}
 
Wallet.prototype.issueCoin = function (kernel, value) {
    var tx = this.simulation.composeIssueTx(kernel, value, this.getUnspentCoins());
    var signedTx = this.signTx(tx);
    this.simulation.addTx(signedTx);
}
 
Wallet.prototype.getUnspentCoins = function () {
    return this.simulation.getUnspentCoins(this.address);
}
 
Wallet.prototype.signTx = function (tx) {
    return tx;  
}

Wallet.prototype.getAddress() {
    return this.address;
}

module.exports = Simulation
