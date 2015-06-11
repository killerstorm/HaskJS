var _ = require('lodash');
var bitcoin = require('bitcoinjs-lib');
var kernel = require('./kernel.js');

function Simulation(name) {
    this.name = name || 'test';
    this.kernel = new kernel.Kernel(this);
    this.transactions = [];
    this.wallets = {};
    this.coins = [];
    this.wallet('uncolored');
}
 
Simulation.prototype.wallet = function (name) {
    this.wallets[name] = new Wallet(this, name);
    return this.wallets[name];
}
 
Simulation.prototype.addTx = function (tx) {
    return this.transactions.push(tx);
}

Simulation.prototype.addCoins = function(coins) {
    this.coins = this.coins.concat(coins);
}

Simulation.prototype.getUnspentCoins = function (addr) {
    var unspent = [];
    var sim = this;
     
    _.map(sim.transactions, function(tx) {
        var index = 0;
        _.each(tx.outs, function(out) {
            if (out.script.chunks.length != 2 &&
                bitcoin.Address.fromOutputScript(out.script).toString() == addr) {
                _.find(sim.coins, function (c) {
                    return (
                        c.txid == tx.getId() &&
                        c.index == index     &&
                        c.value == out.value &&
                        unspent.push(c)
                    );
                });
            }
            index++;
        });
    });
    
    this.coins = _.difference(this.coins, unspent);
    return unspent;                   
}

function Wallet(simulation, name) {
    this.name = name;
    this.simulation = simulation;
    var key = bitcoin.ECKey.makeRandom();
    this.privkey = key;
    this.pubkey  = key.pub;
    this.address = key.pub.getAddress().toString();
    this.coins = [];
}
 
Wallet.prototype.issueCoin = function (value) {
    var tx = this.simulation.kernel.composeIssueTx([{'address' : this.getAddress(), 'value' : value}]);
    tx = this.simulation.kernel.composeBitcoinTx (tx, this.simulation.wallets['uncolored']);
    tx = this.signTx(tx);
    this.simulation.addTx(tx);
    this.simulation.addCoins(_.map(this.simulation.kernel.run([tx]), JSON.parse));
}


Wallet.prototype.send = function (value, target) {
    var tx = this.simulation.kernel.composeSendTx(this.getBalance(),
                                                  [{'address' : target.getAddress(), 'value' : value}],
                                                  this.getAddress());
    tx = this.simulation.kernel.composeBitcoinTx (tx,
                                                  this.simulation.wallets['uncolored'].getUnspentCoins(),
                                                  this.simulation.wallets['uncolored'].getAddress()
                                                 );
    tx = this.signTx(tx);
    this.simulation.addTx(tx);
    this.simulation.addCoins(this.simulation.kernel.run([tx]));
}

Wallet.prototype.getUnspentCoins = function () {
    this.coins = this.coins.concat(this.simulation.getUnspentCoins(this.address));
    return this.coins;
}
 
Wallet.prototype.signTx = function (tx) {
    for (var i = 0; i < tx.ins.length; tx.sign(i, this.privkey), i++);
    return tx;
}

Wallet.prototype.getBalance = function() {
    this.getUnspentCoins();
    return _.sum(this.coins, 'value');
}

Wallet.prototype.getAddress = function() {
    return this.address;
}

module.exports = Simulation
