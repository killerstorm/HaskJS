function init() {
}
    
function run_coin_kernel_on_graph(kernel_name, transactions) {
    

}

function get_mux_shape(kernel_name, payload) {
    console.log(Haste.showpayload(Haste.runCoinKernel(kernel_name)));    

}



exports.run_coin_kernel_on_graph = run_coin_kernel_on_graph;
exports.get_mux_shape            = get_mux_shape;
