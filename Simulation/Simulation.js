function Simulation() {
  this.transactions = [];
  this.wallets = {};
  this.coins = [];
}
 
Simulation.prototype.wallet = function (name) {
  this.wallets[name] = new Wallet(this, name);
  return this.wallets[name];
}
 
Simulation.prototype.addTx = function (tx) {
    
}

Simulation.prototype.getUnspentCoins(addr) {
    //TODO
}

function Wallet(simulation, name) {
  this.simulation = simulation;
  this.privkey = name; 
  this.pubkey =  name; 
  this.address = name; 
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
