const _           = require('lodash');
const bitcoin     = require('bitcoinjs-lib');
const buffertools = require('buffertools');
const Transaction = bitcoin.Transaction;

/**
 * dustThreshold
 * @const {number}
 */
const dustThreshold = 546

/**
 * @return {string} payload
 */
function createPayload (ins, outs, opid, outValues) {
  return '(' +
    JSON.stringify(_.range(ins)) + ', ' +
    JSON.stringify(_.range(outs)) + ', ' +
    outs + ') ' +
    opid.toString() + ' ' + JSON.stringify(outValues)
}
 
function selectCoins (unspentCoins, coinValueFn, neededSum) {
  var total = 0
  
  var selected = _.takeWhile(unspentCoins, function (n) {
    return (total += coinValueFn(n)) >= neededSum
  })

  if (total < neededSum)
    throw new Error ("Not enough coins!")
 
  return selected
}
   
function composeColoredSendTx (wallet, targets, changeAddress) {
  function coinValueFn (coin) {
    return coin.value
  }

  var unspentCoins = wallet.getUnspentCoins()

  targets = _.clone(targets)
  var neededSum = _.sum(targets, 'value')
  var coins     = selectCoins(unspentCoins, coinValueFn, neededSum)
  var inputSum  = _.sum(coins, 'value')
  var change    = inputSum - neededSum   

  wallet.coins = _.difference(wallet.coins, coins)
  if (change > 0) {
    targets.push({address: changeAddress, value: change})
  }
    
  return {inputs: coins, targets: targets}
}
 
function composeColoredIssueTx (targets) {
  return {inputs: [], targets: targets}
}
 
 function composeBitcoinTx (coloredTx, uncoloredWallet) {
  var tx = new Transaction()

  var unspentCoins = uncoloredWallet.getUnspentCoins()
  var changeAddress = uncoloredWallet.getAddress()
  var index = 0
  unspentCoins = _.reject(unspentCoins, 'cv')
 
  var coloredTargets = coloredTx.targets
  var coloredInputs  = coloredTx.inputs
  var fee = 10000
  var uncoloredNeeded   = coloredTargets.length * dustThreshold + fee

   console.log('uncolored needed = ', uncoloredNeeded)
   
  _.each(coloredTargets, function(target) {
    tx.addOutput(target.address, target.value)
    index++
  })
 
  _.each(coloredInputs, function(coin) {
      tx.addInput(coin.txid, coin.index)
      uncoloredNeeded -= coin.value
  })

   console.log('uncolored needed = ', uncoloredNeeded)
  var uncoloredSum = 0
  var uncoloredInputs = []
  var change = 0
    
  if (uncoloredNeeded > 0) {
    uncoloredInputs = selectCoins(
      unspentCoins,
      function (coin) { return coin.value },
      uncoloredNeeded
    )
    
    uncoloredSum        = _.sum(uncoloredInputs, 'value')
    _.each(uncoloredInputs, function(coin) {
      tx.addInput(coin.txid, coin.index)
    })
    change = uncoloredSum - uncoloredNeeded - _.sum(coloredTargets, 'value')
  
    if (change > 0) {
      tx.addOutput(changeAddress, change)
    }
  }

  var outValues = _.pluck(coloredTargets, 'value').concat(
    change == 0 ? [] : [change])
  
  var ins =
    (coloredInputs === [])
    ? coloredInputs.length + uncoloredInputs.length
    : 0

  var outs =
    change
    ? coloredTargets.length + 1
    : coloredTargets.length

  var opid =
    coloredInputs.length
    ? 0
    : 1
  
  var payload = createPayload (ins, outs, opid, outValues)
    
  tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0)
    
  uncoloredWallet.coins = _.difference(uncoloredWallet.coins, uncoloredInputs)

  console.log('out values = ', outValues)
  return {
      'tx' : tx,
      'outValues' : outValues,
      'inputs' : coloredInputs.concat(uncoloredInputs)
  }  
}

module.exports = {
    composeColoredSendTx  : composeColoredSendTx,
    composeBitcoinTx      : composeBitcoinTx,
    composeColoredIssueTx : composeColoredIssueTx
}
