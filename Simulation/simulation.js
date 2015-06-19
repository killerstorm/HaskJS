var _ = require('lodash');
var bitcoin = require('bitcoinjs-lib');
var kernel = require('./kernel.js');

const coinbaseTxId = '0000000000000000000000000000000000000000000000000000000000000000';
<<<<<<< HEAD
const coinbaseOutIndex = 0xFFFFFFFF;
=======
const coinbaseOutIndex = 0;
>>>>>>> develop

function Simulation(name) {
    this.name = name || 'test';
    this.kernel = new kernel.Kernel(this);
    this.transactions = [];
    this.wallets = {};
    this.coins = [];
    this.wallet('uncolored');
    this.init();
}

Simulation.prototype.init = function() {
    var coinbaseTx = new bitcoin.Transaction();
    coinbaseTx.addInput(coinbaseTxId, coinbaseOutIndex);
    coinbaseTx.addOutput(this.wallets['uncolored'].getAddress(), 2500000000);
    coinbaseTx.addOutput(bitcoin.scripts.nullDataOutput(
<<<<<<< HEAD
        new Buffer ('([] [0] 1) 1 [1000000]')), 0);
=======
        new Buffer ('([] [0] 1) 1 [2500000000]')), 0);
>>>>>>> develop
    
    this.addTx(coinbaseTx);
    this.addCoins([{"txid" : coinbaseTx.getId(), "index" : 0, "coinstate" : "2500000000", "value" : 2500000000}]);
    
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
<<<<<<< HEAD
     
=======
    
>>>>>>> develop
    _.map(sim.transactions, function(tx) {
        var index = 0;
        _.each(tx.outs, function(out) {
            if (out.script.chunks.length != 2 &&
                bitcoin.Address.fromOutputScript(out.script).toString() == addr) {
                _.find(sim.coins, function (c) {
                    return (
                        c.txid == tx.getId() &&
                        c.index == index     &&
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
    this.colored 
    this.simulation = simulation;
    var key = bitcoin.ECKey.makeRandom();
    this.privkey = key;
    this.pubkey  = key.pub;
    this.address = key.pub.getAddress().toString();
    this.coins = [];
}
 
Wallet.prototype.issueCoin = function (value) {
    var coloredTx = this.simulation.kernel.composeIssueTx([{'address' : this.getAddress(), 'value' : value}]);
<<<<<<< HEAD
    //tc => tx + coins , { 'tx' : tx, 'coins' : coins }
    var tc = this.simulation.kernel.composeBitcoinTx (coloredTx, this.simulation.wallets['uncolored']);
    var tx = this.signTx(tc.tx);
    this.simulation.addTx(tx);
    var txid = tx.getId();
    this.simulation.addCoins(_.each(tc.coins, function (n) { _.set(n, 'txid', txid);}));
=======
    //txio => {tx, inputs, outValues}
    var txio = this.simulation.kernel.composeBitcoinTx (coloredTx, this.simulation.wallets['uncolored']);
    var tx = this.signTx(txio.tx);
    this.simulation.addTx(tx);
    this.simulation.addCoins(this.simulation.kernel.runKernel(tx, txio.inputs, txio.outValues));
>>>>>>> develop
}


Wallet.prototype.send = function (value, target) {
    var coloredTx = this.simulation.kernel.composeSendTx(this,
                                                  [{'address' : target.getAddress(), 'value' : value}],
<<<<<<< HEAD
                                                  this.getAddress());
    var tc = this.simulation.kernel.composeBitcoinTx (coloredTx, this.simulation.wallets['uncolored']);
    var tx = this.signTx(tc.tx);
    this.simulation.addTx(tx);
    var txid = tx.getId();
    this.simulation.addCoins(_.each(tc.coins, function (n) { _.set(n, 'txid', txid);}));
=======
                                                         this.getAddress());
    //txio => {tx, inputs, outValues}
    var txio = this.simulation.kernel.composeBitcoinTx (coloredTx, this.simulation.wallets['uncolored']);
    var tx = this.signTx(txio.tx);
    this.simulation.addTx(tx);
    this.simulation.addCoins(this.simulation.kernel.runKernel(tx, txio.inputs, txio.outValues));
>>>>>>> develop
}

Wallet.prototype.getUnspentCoins = function () {
    this.coins = this.coins.concat(this.simulation.getUnspentCoins(this.address));
    return this.coins;
}
 
Wallet.prototype.signTx = function (tx) {
    var txb = new bitcoin.TransactionBuilder.fromTransaction(tx);
    for (var i = 0; i < txb.tx.ins.length; txb.sign(i, this.privkey), i++);
    tx = txb.build();
    return tx;
}

Wallet.prototype.getBalance = function() {
    this.getUnspentCoins();
    return _.sum(this.coins, "value");
}

Wallet.prototype.getAddress = function() {
    return this.address;
}

module.exports = Simulation
