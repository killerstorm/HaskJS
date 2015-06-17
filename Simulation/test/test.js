var Simulation = require('../simulation.js')
var kernel     = require('../kernel.js')
var bitcoin    = require('bitcoinjs-lib')
var _          = require('lodash')
var expect     = require('chai').expect



describe("Simulation", function() {
    var sim;
    var uncolored;
    var alice;
    var bob;
    
    it("'new Simulation()' should return new simulation object", function() {
        sim = new Simulation();
        expect(sim).to.have.a.property('name', 'test');
        expect(sim).to.have.a.property('transactions');
        
    });
    
    it("Uncolored wallet is initialised by default", function() {
        expect(sim.wallets).to.have.a.property('uncolored');
        uncolored = sim.wallets['uncolored'];
    });

    describe("Wallets initialization", function() {
        it("Alice's wallet initialisation", function() {
            alice = sim.wallet('alice');
            expect(Object.keys(sim.wallets).length).to.equal(2);
        });

        it("Bob's wallet initialisation", function() {
            bob = sim.wallet('bob');
            expect(Object.keys(sim.wallets).length).to.equal(3);
        });
    });

    describe("Wallet operations", function() {
        it("Get 'uncolored' balance, expected to equal 25 bitcoins (2500000000) by default", function() {
            var uncoloredBalance = uncolored.getBalance();
            expect(uncoloredBalance).to.equal(2500000000);
        });
        
        describe("Issue coin", function() {
            it("Alice issues 10 bitcoins coin", function() {
                alice.issueCoin(1000000000);
            });
        
            it("Bob issues 20 bitcoins coin", function() {
                bob.issueCoin(2000000000);
            });
        });
        
        describe("Get balance", function() {
            it("Alice's balance, expected to equal 1000000000 satoshi", function() {
                var balance = alice.getBalance();
                expect(balance).to.equal(1000000000);
            });

            it("Bob's balance, expected to equal 2000000000 satoshi", function() {
                var balance = bob.getBalance();
                expect(balance).to.equal(2000000000);
            });
        });

        describe("Send", function() {
            it("Alice sends 1000000 satoshi to Bob", function() {
                alice.send(1000000, bob);
            });

            it ("Alice's balance = 999000000", function() {
                expect(alice.getBalance()).to.equal(999000000);
            })

            it("Bob sends 1000000000 satoshi to Alice", function() {
                bob.send(1000000000, alice);
            });

            it ("Bob's balance = 1001000000", function() {
                expect(bob.getBalance()).to.equal(1001000000);
            });

        });
        
    });
     
});


