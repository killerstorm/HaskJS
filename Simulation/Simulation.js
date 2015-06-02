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
    //TODO
    // get target address from txid???
    // filter coins from this.coins by out[n].address == addr
    
}

function Wallet(simulation) {
    this.simulation = simulation;
    var key = bitoin.ECKey.makeRandom();
    this.privkey = key;
    this.pubkey  = key.pub;
    this.address = key.pub.getAddress();
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
    // TODO
    // nobody's going to check if we have signed it so unsigned works too
    return tx;  
}

Wallet.prototype.getAddress() {
    return this.address;
}

module.exports = Simulation