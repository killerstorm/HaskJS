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

module.exports = Simulation
