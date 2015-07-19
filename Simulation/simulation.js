var _           = require('lodash')
var bitcoin     = require('bitcoinjs-lib')
var kernel      = require('./kernel.js')
var base58      = require('base58-native')
var compose     = require('./composeTx.js')
var bc          = require('./bitcoinclient')
var Promise     = require('bluebird')
var testdata    = require('./testdata')

var Color       = kernel.Color
var ColorValue  = kernel.ColorValue
var Kernel      = kernel.Kernel
var sha256      = bitcoin.crypto.sha256
var Transaction = bitcoin.Transaction

var network = bitcoin.networks.testnet

/**
 * fee
 * @const
 */
const fee = 10000

/**
 * 0x80 byte
 * @const
 */
const x80 = new Buffer('80', 'hex')

/**
 * 0x01 byte
 * @const
 */
const x01 = new Buffer('01', 'hex')

function init (sim) {
  sim.wallet('bitcoin')
  sim.transactions = _.map(testdata.transactions, Transaction.fromHex)

  _.each(sim.transactions, function (tx) {
    var txid = tx.getId()
    for (var i = 0; i < tx.outs.length; i++) {
      var value = tx.outs[i].value
      sim.coins.push({
        "txid" : txid
      , "index" : i
      , "coinstate" : value.toString()
      , "value" : value
      })
    }
  })
}

/**
 * Simulation
 * @constructor
 */
function Simulation(name) {
  this.name          = name || 'test'
  this.transactions  = []
  this.wallets       = {}
  this.coins         = []
  init(this)
}


/**
 * kernel
 */
Simulation.prototype.kernel = function(kernelName) {
  var kernel = new Kernel(this, kernelName)
  return kernel
}

/**
 * @return {Wallet}
 */
Simulation.prototype.wallet = function (name) {
  this.wallets[name] = new Wallet(this, name)
  return this.wallets[name]
}

Simulation.prototype.addTx = function (tx) {
  return this.transactions.push(tx)
}

Simulation.prototype.addCoins = function (coins) {
  this.coins = this.coins.concat(coins)
}

Simulation.prototype.getUnspentCoins = function (addr) {
  var unspent = []
  var sim = this
  _.map(sim.transactions, function (tx) {
    var index = 0
    _.each(tx.outs, function (out) {
      if (out.script.chunks.length != 2 &&
          getOutputAddress(out.script) == addr) {
        _.find(sim.coins, function (c) {
                return (c.txid == tx.getId() &&
                        c.index == index     &&
                        unspent.push(c))
            })
        }
      index++
    })
  })
  this.coins = _.difference(this.coins, unspent)
  return unspent
}

/**
 * Wallet
 * @interface
 */
function Wallet(simulation, name) {
  this.name       = name || 'test'
  this.simulation = simulation
  this.key        = bitcoin.crypto.sha256(name)
  this.wif        = this.getWIF()
  this.privkey    = bitcoin.ECKey.fromWIF(this.wif)
  this.pubkey     = this.privkey.pub
  this.address    = this.pubkey.getAddress(network).toString()
  this.coins      = []
}

/**
 * Get WIF of given private key
 * @return {string} WIF
 */
Wallet.prototype.getWIF = function () {
  var extkey   = new Buffer.concat([x80, this.key, x01])
  var checksum = (sha256(sha256(extkey))).slice(0, 4)
  var wif      = base58.encode(new Buffer.concat([extkey, checksum]))
  
  return wif
}

/**
 * Issue coin
 *
 */
Wallet.prototype.issueCoin = function (kernel, value) {
  const sim = this.simulation
  
  var coloredTx = compose.composeColoredIssueTx(
    [{ 'address': this.getAddress(), 'value': value }]
  )
  
  var tx = compose.composeBitcoinTx(
    coloredTx, this.getUnspentCoins(), this.getAddress()
  )
  
  tx   = this.signTx(tx)
  
  sim.addTx(tx)   
  var coins = kernel.runKernel(tx)
  var color = new Color(kernel, tx.getId())

  coins[0].cv = new ColorValue (color, coins[0].value)

  sim.addCoins(coins)

  return color
}

/**
 * Get bitcoins
 */
Wallet.prototype.getCoins = function (amount) {
  var bitcoinWallet = this.simulation.wallets['bitcoin']
  var neededAmount = amount + fee
  var unspentCoins = bitcoinWallet.getUnspentCoins()
  var selectedCoins  = compose.selectCoins (
    unspentCoins,
    function (n) { return n.value },
    neededAmount
  )

  compose.removeSpentCoins (unspentCoins, selectedCoins)
  
  var tx = new Transaction()

  _.each(selectedCoins, function (coin) {
    tx.addInput(coin.txid, coin.index)
  })

  tx.addOutput(this.getAddress(), amount) 
  var change = _.sum(selectedCoins, 'value') - neededAmount

  if (change > 0)
    tx.addOutput(bitcoinWallet.getAddress(), change)

  tx = bitcoinWallet.signTx(tx)

  var coins = []
  for (var i = 0; i < tx.outs.length; i++) {
    coins.push({
      txid : tx.getId()
    , index : i
    , coinstate : tx.outs[i].value.toString()
    , value : tx.outs[i].value
    })
  }

  this.simulation.addTx(tx)
  this.simulation.addCoins(coins)
    
}

/**
 * Send
 */
Wallet.prototype.send = function (colorValue, target) {
  var coloredTx = compose.composeColoredSendTx(
    this.getUnspentCoins(),
    [{ 'address': target.getAddress(), 'value': colorValue.value }],
    this.getAddress()
  )
  var tx = compose.composeBitcoinTx(
    coloredTx, this.getUnspentCoins(), this.getAddress()
  )
  
  tx = this.signTx(tx)
  this.simulation.addTx(tx)
  this.simulation.addCoins(
    colorValue.color.kernel.runCoinKernelOnGraph(tx)
  )
}


Wallet.prototype.getUnspentCoins = function () {
  this.coins = this.coins.concat(
    this.simulation.getUnspentCoins(this.address)
  )
  return this.coins
}

/**
 * Sign transaction
 * @return {bitcoin.Transaction}
 */
Wallet.prototype.signTx = function (tx) {
  var txb = new bitcoin.TransactionBuilder.fromTransaction(tx)
  for (var i = 0; i < txb.tx.ins.length; txb.sign(i, this.privkey), i++) { }
  tx = txb.build()
  return tx
}

/**
 * @return {number} balance
 */
Wallet.prototype.getBalance = function () {
  this.getUnspentCoins()
  return _.sum(this.coins, "value")
}

/**
 * Get Wallet's address
 * @return {string} address
 */
Wallet.prototype.getAddress = function () {
  return this.address
};

/**
 *Get address from output script
 *@return {string} address
 */

function getOutputAddress (outScript) {
  return bitcoin.Address.fromOutputScript(outScript, network).toString()
}
  
module.exports = Simulation
