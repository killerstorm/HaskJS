var ckernel = require ('./main.js');
const bc = require('bitcoinjs-lib');
const bt = require('buffertools');
var Transaction = bc.Transaction;
var _ = require('underscore');


var h = ckernel.getHaste()

var t = Transaction.fromHex('0100000001993a052baad64bccccb3c32f1603e95457949c33208052ba6893a1c8fc3bfadb000000006b483045022100d38cd30a2b34275b1f4054d56e7ab35a87cc0cd83e7ae4747e641fd696f4b764022076017c72dab2cef85016330d2a1798c5820955708adfcb23410caef92d3b58c40121022539abaecacca3e643bdadfd5f34f52e39fc1c5d6863536fb987d37999a55cdeffffffff0458020000000000001976a914cadeb078e53c27c75e3e1d9e6399ef90cafc456b88ac0000000000000000096a074f410100010900104e3800000000001976a9148e9c9a0187f0da5c69f4d7d35cf40e862e14653a88ac28180600000000001976a914cadeb078e53c27c75e3e1d9e6399ef90cafc456b88ac00000000');



var test_tx = ckernel.createTx(t);
var transactions = [];
transactions.push(test_tx);



var coins = [];


console.log(JSON.stringify(h["runCoinKernelOnGraph"](transactions)));


    
   
