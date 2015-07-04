var _           = require('lodash')
var bitcoin     = require('bitcoinjs-lib')
var kernel      = require('./kernel.js')
var base58      = require('base58-native')
var composetx   = require('./composeTx.js')
var bc          = require('bitcoin')
var Promise     = require('bluebird')

var sha256      = bitcoin.crypto.sha256
var Transaction = bitcoin.Transaction
var client      = new bc.Client({
  host : 'localhost'
, port : 18332
, user : 'user'
, pass : 'password'
})

var network = bitcoin.networks.testnet

/**
 * BTC
 * @const
 */
const BTC = 100000000

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

/**
 * ============================================================
 * Promisified RPC functions
 * 
 * ============================================================
 */
function generate(n) {
  n = (n === undefined) ? 1 : n
  
  return new Promise (function (resolve, reject) {
    client.setGenerate (true, n, function (err, res) {
      if (err)
        reject (err)
      else {
        resolve (n)
      }
    })
  })
}

function sendtoaddress (address, amount) {
  return new Promise (function (resolve, reject) {
    client.sendToAddress (address, amount, function (err, txid) {
      if (err)
        reject (err)
      else
        resolve (txid)
    })
  })
}

function listtransactions (name) {
  name = (name === undefined) ? "" : name
  
  return new Promise (function (resolve, reject) {
    client.listTransactions (name, function (err, res) {
      if (err)
        reject (err)
      else 
        resolve (res)
    })
  })
}

function getrawtransaction (txid, sim) {
  return new Promise (function (resolve, reject) {
    client.getRawTransaction (txid, function (err, txHex) {
      if (err)
        reject (err)
      else {
        sim.transactions.push(Transaction.fromHex(txHex)) 
        resolve(txHex)
      }
    })
  })
}

function decoderawtransaction (rawtx) {
  return new Promise (function (resolve, reject) {
    client.decodeRawTransaction (rawtx, function (err, tx) {
      if (err)
        reject (err)
      else 
        resolve (tx)
    })
  })
}

function gettransaction (txid) {
  return new Promise (function (resolve, reject) {
    client.getTransaction (txid, function (err, tx) {
      if (err)
        reject (err)
      else {
        resolve(tx)
      }
    })
  })
}

/**
 * ============================================================
 */

/**
 * Simulation
 * @interface
*/
function Simulation(name) {
  this.isInitialized = false
  this.name          = name || 'test'
  this.kernel        = new kernel.Kernel(this)
  this.transactions  = []
  this.wallets       = {}
  this.coins         = []
}

/**
 * Simulation initialize function
 */
Simulation.prototype.init = function () {
  if (this.isInitialized) {
    console.log("simulation already initialized!")
    return
  }

  var sim = this
 
  generate (1)
  .then(function ()  {
    return listtransactions()
  })
  .each(function (t) {
    return getrawtransaction (t.txid, sim)
  }) 
  .then(function ()  {
    sim.isInitialized = true
    console.log ('initialization successfull!')
  })
}

/**
 * @return {Wallet}
 */
Simulation.prototype.wallet = function (name) {
    this.wallets[name] = new Wallet(this, name);
    return this.wallets[name];
};

Simulation.prototype.addTx = function (tx) {
    return this.transactions.push(tx);
};

Simulation.prototype.addCoins = function (coins) {
    this.coins = this.coins.concat(coins);
};

Simulation.prototype.getUnspentCoins = function (addr) {
  var unspent = []
  var sim = this
  _.map(sim.transactions, function (tx) {
    var index = 0
    _.each(tx.outs, function (out) {
      if (out.script.chunks.length != 2 &&
          bitcoin.Address.fromOutputScript(out.script, network).toString() == addr) {
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
Wallet.prototype.issueCoin = function (value) {
  var coloredTx = this.simulation.kernel.composeIssueTx(
    [{ 'address': this.getAddress(), 'value': value }]
  )
  
  var txio = this.simulation.kernel.composeBitcoinTx(
    coloredTx, this
  )
  
  var tx = this.signTx(txio.tx)
  this.simulation.addTx(tx)
  this.simulation.addCoins(
    this.simulation.kernel.runKernel(tx, txio.inputs, txio.outValues)
  )
}

/**
 * Get bitcoins
 */
Wallet.prototype.getcoins = function (amount) {
  var wallet  = this
  var address = this.getAddress()
  var id      = null
  sendtoaddress (this.getAddress(), amount)
  .then(function (txid) {
    id = txid
    return getrawtransaction (txid, wallet.simulation)
  })
  .then(function (rawtx) {
    return decoderawtransaction (rawtx)
  })
  .then(function (tx) {
    return new Promise (function (resolve) {
      resolve (tx.vout)
    })
  })
  .each(function (out) {
    return new Promise (function (resolve) {
      if (out.scriptPubKey.addresses[0] === address) {
        wallet.coins.push({
          txid      : id
        , index     : out.n
        , coinstate : (out.value * BTC).toString()
        , value     : out.value * BTC
        })
      }
      resolve(out)
    })
  })
  .then(function (tx) {
    console.log("got coins")
  })
}

/**
 * Send
 */
Wallet.prototype.send = function (value, target) {
  var coloredTx = this.simulation.kernel.composeSendTx(
    this,
    [{ 'address': target.getAddress(), 'value': value }],
    this.getAddress()
  )
  
  var txio = this.simulation.kernel.composeBitcoinTx(
    coloredTx, this.simulation.wallets['uncolored']
  )
  
  var tx = this.signTx(txio.tx)
  this.simulation.addTx(tx)
  this.simulation.addCoins(
    this.simulation.kernel.runKernel(tx, txio.inputs, txio.outValues)
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
};

/**
 * @return {string} address
 */
Wallet.prototype.getAddress = function () {
    return this.address
};

module.exports = Simulation
