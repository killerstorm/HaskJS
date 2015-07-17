var bc          = require('bitcoin')
var Promise     = require('bluebird')

function BitcoinClient() {
  this.client =  new bc.Client({
    host : 'localhost'
  , port : 18332
  , user : 'user'
  , pass : 'password'
  })
}

/**
 * ============================================================
 * Promisified RPC functions
 * 
 * ============================================================
 */
BitcoinClient.prototype.setgenerate = function (n) {
  n = (n === undefined) ? 1 : n
  
  return new Promise (function (resolve, reject) {
    this.client.setGenerate (true, n, function (err, res) {
      if (err)
        reject (err)
      else {
        resolve (n)
      }
    })
  })
}

BitcoinClient.prototype.sendtoaddress  = function (address, amount) {
  return new Promise (function (resolve, reject) {
    this.client.sendToAddress (address, amount, function (err, txid) {
      if (err)
        reject (err)
      else
        resolve (txid)
    })
  })
}

BitcoinClient.prototype.listtransactions = function (name) {
  name = (name === undefined) ? "" : name
  
  return new Promise (function (resolve, reject) {
    this.client.listTransactions (name, function (err, res) {
      if (err)
        reject (err)
      else 
        resolve (res)
    })
  })
}

BitcoinClient.prototype.getrawtransaction = function (txid, sim) {
  return new Promise (function (resolve, reject) {
    this.client.getRawTransaction (txid, function (err, txHex) {
      if (err)
        reject (err)
      else {
        sim.transactions.push(Transaction.fromHex(txHex)) 
        resolve(txHex)
      }
    })
  })
}

BitcoinClient.prototype.decoderawtransaction = function (rawtx) {
  return new Promise (function (resolve, reject) {
    this.client.decodeRawTransaction (rawtx, function (err, tx) {
      if (err)
        reject (err)
      else 
        resolve (tx)
    })
  })
}

BitcoinClient.prototype.gettransaction = function (txid) {
  return new Promise (function (resolve, reject) {
    this.client.getTransaction (txid, function (err, tx) {
      if (err)
        reject (err)
      else {
        resolve(tx)
      }
    })
  })
}

BitcoinClient.prototype.sendrawtransaction = function (rawTx) {
  return new Promise (function (resolve, reject) {
    this.client.sendRawTransaction (rawTx, function (err, tx) {
      if (err)
        reject (err)
      else
        resolve (tx)
    })
  })
}

module.exports = BitcoinClient