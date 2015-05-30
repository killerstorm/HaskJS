function Wallet(simulation, privKey, pubKey, addr) {
  this.simulation = simulation;
  this.privkey = privKey; 
  this.pubkey = pubKey; 
  this.address = addr; 
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


module.exports = Wallet
