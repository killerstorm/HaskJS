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
 
function Wallet(simulation, name) {
  this.simulation = simulation;
  this.privkey = name; // TODO
  this.pubkey = name; // TODO
  this.address = name; // TODO
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
